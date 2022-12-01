package io.opsit.explang.parser.alg;

import static io.opsit.explang.Utils.astnize;
import static io.opsit.explang.Utils.coalesce;
import static io.opsit.explang.Utils.map;
import static io.opsit.explang.Utils.set;
import static io.opsit.explang.Utils.symbol;
import static io.opsit.explang.autosuggest.LanguageToken.TOKKIND_BOOLEAN;
import static io.opsit.explang.autosuggest.LanguageToken.TOKKIND_COMMENTS;
import static io.opsit.explang.autosuggest.LanguageToken.TOKKIND_KEYWORDS;
import static io.opsit.explang.autosuggest.LanguageToken.TOKKIND_NIL;
import static io.opsit.explang.autosuggest.LanguageToken.TOKKIND_NUMBER;
import static io.opsit.explang.autosuggest.LanguageToken.TOKKIND_OPERATOR;
import static io.opsit.explang.autosuggest.LanguageToken.TOKKIND_PARENTHESES;
import static io.opsit.explang.autosuggest.LanguageToken.TOKKIND_SYMBOL;
import static io.opsit.explang.autosuggest.LanguageToken.TOKKIND_WHITESPACE;

import com.vmware.antlr4c3.CodeCompletionCore;
import com.vmware.antlr4c3.CodeCompletionCore.CandidatesCollection;
import io.opsit.explang.ASTN;
import io.opsit.explang.ASTNLeaf;
import io.opsit.explang.ASTNList;
import io.opsit.explang.ArgSpec;
import io.opsit.explang.Compiler;
import io.opsit.explang.ICode;
import io.opsit.explang.IParser;
import io.opsit.explang.Keyword;
import io.opsit.explang.OperatorDesc;
import io.opsit.explang.ParseCtx;
import io.opsit.explang.ParserException;
import io.opsit.explang.ParserExceptions;
import io.opsit.explang.Symbol;
import io.opsit.explang.atom.AtomParseException;
import io.opsit.explang.atom.AtomParser;
import io.opsit.explang.atom.BooleanParser;
import io.opsit.explang.atom.EscStringParser;
import io.opsit.explang.atom.NullParser;
import io.opsit.explang.atom.NumberParser;
import io.opsit.explang.atom.RegexpParser;
import io.opsit.explang.atom.SymbolParser;
import io.opsit.explang.atom.SymfuncParser;
import io.opsit.explang.atom.VersionParser;
import io.opsit.explang.autosuggest.IAutoSuggester;
import io.opsit.explang.autosuggest.LanguageToken;
import io.opsit.explang.autosuggest.SourceInfo;
import io.opsit.explang.autosuggest.Suggestion;
import io.opsit.explang.autosuggest.Tokenization;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.misc.Utils;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.RuleNode;
import org.antlr.v4.runtime.tree.TerminalNode;

@SuppressWarnings({"unchecked", "rawtypes", "serial"})
public class AlgParser implements IParser, IAutoSuggester {
  public static final String DESCRIPTION_KEY = "description";
  public static final String DISPLAYNAME_KEY = "displayName";

  @Override
  public ASTNList parse(ParseCtx pctx, String input) {
    return parse(pctx, input, Integer.MAX_VALUE);
  }

  @Override
  public ASTNList parse(ParseCtx pctx, String input, int maxExprs) {
    final InputStream is = new ByteArrayInputStream(input.getBytes());
    return parse(pctx, is, maxExprs);
  }

  public ASTNList parse(ParseCtx pctx, InputStream is, int maxExprs) {
    final Reader reader = new InputStreamReader(is);
    return parse(pctx, reader, maxExprs);
  }

  @Override
  public ASTNList parse(ParseCtx pctx, Reader reader, int maxExprs) {
    ParserException globalProblem = null;
    ASTNList result = null;
    try {
      ParsingState pst = mkParsingState(reader);
      Exception problem = null;
      final List<SyntaxError> syntaxErrors = pst.listener.getSyntaxErrors();
      ExprVisitor visitor = new ExprVisitor(pctx.input);
      if (null != syntaxErrors && syntaxErrors.size() > 0) {
        problem =
            new ParserExceptions(
                mkPctx(pctx, syntaxErrors.get(0)),
                syntaxErrors.stream()
                    .map(se -> se2ParserException(pctx, se))
                    .collect(Collectors.toList()));
        try {
          // attempt error recovery and return some tree anyway
          result = (ASTNList) visitor.visit(pst.tree);
        } catch (Exception ex) {
          // nothing to do
        }
      } else {
        result = (ASTNList) visitor.visit(pst.tree);
      }
      if (null == result) {
        result = new ASTNList(list(), pctx.clone());
      }
      if (null != problem) {
        result.problem = problem;
      }
    } catch (Exception ex) {
      globalProblem = new ParserException(pctx.clone(),
                                          "AlgParser Exception: "
                                          + ex.getMessage(),
                                          ex);
    }
    ASTNList resultList = null == result ? new ASTNList(list(), pctx.clone()) : result;
    if (maxExprs < resultList.size()) {
      for (int i = resultList.size() - 1; i >= maxExprs; i--) {
        resultList.getList().remove(i);
      }
    }
    if (null != globalProblem) {
      resultList.problem = globalProblem;
    }
    resultList.setMultiExpr(true);
    return resultList;
  }

  public boolean supportREPLStream() {
    return false;
  }

  private static class ParsingState {
    public ParseTree tree;
    public AlgParserParser parser;
    public CommonTokenStream tokenStream;
    public SyntaxErrorListener listener;

    public ParsingState(
        AlgParserParser parser,
        ParseTree tree,
        CommonTokenStream tokenStream,
        SyntaxErrorListener listener) {
      this.parser = parser;
      this.tree = tree;
      this.tokenStream = tokenStream;
      this.listener = listener;
    }
  }

  private ParsingState mkParsingState(String inputStr) {
    final InputStream is = new ByteArrayInputStream(inputStr.getBytes());
    final Reader reader = new InputStreamReader(is);
    return mkParsingState(reader);
  }

  private ParsingState mkParsingState(Reader reader) {
    try {
      // ANTLRInputStream input = new  ANTLRInputStream(reader,1,1);
      CharStream inputStream = CharStreams.fromReader(reader);

      SyntaxErrorListener listener = new SyntaxErrorListener();
      // create a lexer that feeds off of input CharStream​
      AlgParserLexer lexer = new AlgParserLexer(inputStream);
      lexer.removeErrorListeners();
      lexer.addErrorListener(listener);
      // create a buffer of tokens pulled from the lexer​
      // FIXME: customize type of token stream
      CommonTokenStream tokenStream = new CommonTokenStream(lexer);
      // TokenStream tokens = new UnbufferedTokenStream(lexer);
      // create a parser that feeds off the tokens buffer​
      AlgParserParser parser = new AlgParserParser(tokenStream);
      parser.removeErrorListeners();
      parser.addErrorListener(listener);
      ParseTree tree = parser.replblock(); // begin parsing at init rule​*/
      return new ParsingState(parser, tree, tokenStream, listener);
    } catch (IOException ex) {
      throw new RuntimeException("IOException while parsing", ex);
    }
  }

  public List<String> getErrors(String inputStr, int curPos) {
    return getErrors(mkParsingState(inputStr), curPos);
  }

  public List<String> getErrors(ParsingState state, int curPos) {
    return state.listener.syntaxErrors.stream().map(e -> e.toString()).collect(Collectors.toList());
  }

  public Tokenization tokenize(String inputStr, int curPos) {
    return tokenize(mkParsingState(inputStr), curPos);
  }

  /**
   * Return tokenization of expression including information on current token (with active cursor).
   */
  public Tokenization tokenize(ParsingState state, int curPos) {
    final Integer tokIdx = computeTokenIndex(state.tree, state.tokenStream, curPos);
    final List<LanguageToken> languageTokens = list();
    final List<Token> tokens = state.tokenStream.getTokens();
    String tokenStr = "";
    int tokenPos = 0;
    for (int i = 0; i < tokens.size(); i++) {
      Token token = tokens.get(i);
      languageTokens.add(
          new LanguageToken(
              token.getText(), getTokenKind(token.getType()), token.getCharPositionInLine()));
      if (null != tokIdx && i == tokIdx.intValue()) {
        tokenStr = token.getText();
        tokenPos = curPos - token.getCharPositionInLine();
      }
    }
    return new Tokenization(languageTokens, tokIdx, tokenStr, tokenPos);
  }

  // public boolean checkCandidate(Integer tokType, List<Integer> toks, Tokenization t) {
  // return false;
  // }
  OperatorDesc[] operatorDescs = {
    new OperatorDesc("+", "+", "a + b"),
    new OperatorDesc("-", "-", "a - b", "- x"),
    new OperatorDesc("*", "*", "a * b"),
    new OperatorDesc("/", "/", "a / b"),
    new OperatorDesc("%", "%", "a % b"),
    new OperatorDesc("<", "<", "a < b"),
    new OperatorDesc("<=", "<=", "a <= b"),
    new OperatorDesc(">", ">", "a > b"),
    new OperatorDesc(">=", ">=", "a >= b"),
    new OperatorDesc("!=", "!=", "a != b"),
    new OperatorDesc("==", "==", "a == b"),
    new OperatorDesc("===", "===", "a === b"),
    new OperatorDesc("=", "=", "a = b"),
    new OperatorDesc("in", "IN", "a in [1, 2, 3]", "h in \"hello\""),
    new OperatorDesc(":=", "SETF",
                     "var := expr;",
                     "[var1, var2, var3] := [ val1, val2, val3];",
                     "[var1, [var2, var3]] := [ val1, [val2, val3]];",
                     "{var1 : \"key1\", var2 : \"key2\"} := {\"key2\" : val2, \"key1\" : val1};"),
    new OperatorDesc(":=", "SETL",
                     "local var := expr;"),
    new OperatorDesc(":=", "SETQ",
                     "global var := expr;"),
    new OperatorDesc("not", "NOT", "not x"),
    new OperatorDesc("and", "AND", "x and b"),
    new OperatorDesc("or",  "OR", "x or b"),
    new OperatorDesc("while", "WHILE",
                     ""
                     + "while (conditional expr)\n"
                     + "  expression;\n"
                     + "  ...\n"
                     + "end\n"),
    new OperatorDesc("if", "COND", ""
                     + "if condition\n"
                     + "  expr;\n"
                     + "  ...\n"
                     + "end\n",
                     ""
                     + "if condition1\n"
                     + "  expr;\n"
                     + "  ...\n"
                     + "elseif condition2\n"
                     + "  expr;\n"
                     + "  ...\n"
                     + "else\n"
                     + "  expr;\n"
                     + "  ...\n"
                     + "end\n"),
    new OperatorDesc("let", "LET", ""
                     + "let var1 := val1, var2, var3:=val3\n"
                     + "  expr;\n"
                     + "  ...\n"
                     + "end\n"),
    new OperatorDesc("return", "RETURN", "return value;"),
    new OperatorDesc(".", "GET_IN", "a.b", "a.b.c"),
    new OperatorDesc("[]", "GET_IN", "a[1]", "a[1][2]","b[\"foo\"]"),
    new OperatorDesc("begin","PROGN", ""
                     + "begin\n"
                     + "  expr;\n"
                     + "  ...\n"
                     + "end"),
    new OperatorDesc("search","SEARCH",
                     "[val1, val2, val3] SEARCH r\"regexp\"",
                     "[val1, val2, val3] SEARCH \"substring\"",
                     "[val1, val2, val3] SEARCH \"substring\"",
                     "[val1, val2, val3] SEARCH atom",
                     "[val1, val2, val3] SEARCH predicate_expression"),
    new OperatorDesc("=~","DWIM_MATCHES",
                     "value =~ r\"regexp\"",
                     "value =~ \"substring\"",
                     "value =~ value2",
                     "value =~ predicate_expression"),
    new OperatorDesc("for", "FOREACH", ""
                     + "FOR var IN sequence\n"
                     + "  expr;\n"
                     + "  ...\n"
                     + "end",
                     ""
                     + "FOR var IN sequence RESULT expr\n"
                     + "  expr;\n"
                     + "  ...\n"
                     + "end"),
    new OperatorDesc("function", "DEFUN", ""
                     + "function symbol ( arglist )\n"
                     + "  expr;\n"
                     + "  ...\n"
                     + "end;"),
    new OperatorDesc("function", "LAMBDA", ""
                     + "function ( arglist )\n"
                     + "  expr;\n"
                     + "  ...\n"
                     + "end"),
    new OperatorDesc("->", "LAMBDA", "( arglist ) -> expr", "var -> expr"),
    new OperatorDesc("try", "TRY", ""
                     + "try\n"
                     + "   expr;\n"
                     + "   ...\n"
                     + "catch (some.Exception ex)\n"
                     + "   expr;\n"
                     + "   ...\n"
                     + "catch ex\n"
                     + "   expr;\n"
                     + "   ...\n"                     
                     + "finally\n"
                     + "   expr;\n"
                     + "   ...\n"
                     + "end")
  };


  @Override
  public OperatorDesc[] getOperatorDescs() {
    return operatorDescs;
  }



  @Override
  public SourceInfo autoSuggest(
      String inputStr,
      Compiler.ICtx ctx,
      int curPos,
      boolean returnTokenization,
      boolean returnErrors,
      boolean returnSuggestions,
      boolean filterSuggestions) {
    final SourceInfo results = new SourceInfo(inputStr, curPos);
    try {
      ParsingState st = mkParsingState(inputStr);
      results.tokenization = tokenize(inputStr, curPos);
      if (null != results.tokenization.tokenIndex) {
        int tokIdx = results.tokenization.tokenIndex;
        String[] ruleNames = st.parser.getRuleNames();
        Set<Integer> preferredRules = set();
        for (int j = 0; j < ruleNames.length; j++) {
          preferredRules.add(st.parser.getRuleIndex(inputStr));
        }
        CodeCompletionCore ccc =
            new CodeCompletionCore(
                st.parser,
                preferredRules, // preferred rules
                set()); // ignored tokens
        CandidatesCollection candidates = ccc.collectCandidates(tokIdx, null); // ParserRuleContext
        for (Map.Entry<Integer, List<Integer>> entry : candidates.tokens.entrySet()) {
          results.addAll(
              mkSuggestionForToken(
                  entry.getKey(), entry.getValue(), ctx, filterSuggestions, results.tokenization));
        }
      } else {
        results.errors.add("No token matches character position: " + curPos);
      }
    } catch (Exception ioex) {
      throw new RuntimeException("Error in auto suggester: " + ioex.toString());
    }
    return results;
  }

  /** Unquote quoted expression. */
  public String unquote(String str) {
    if (null == str) {
      return null;
    }
    return str.replaceAll("^'+", "").replaceAll("'+$", "");
  }

  /**
   * Format function docstring for display.
   */
  public String docstringSummary(String str) {
    if (null == str) {
      return null;
    }
    int dotidx = str.indexOf(".");
    int nlidx = str.indexOf("\n");
    int idx = str.length();
    if (dotidx > 0 && dotidx < idx) {
      idx = dotidx;
    }
    if (nlidx > 0 && nlidx < idx) {
      idx = nlidx;
    }
    return str.substring(0, idx).trim();
  }

  /**
   * Return type of callable object.
   */
  public String codeType(ICode code) {
    StringBuilder buf = new StringBuilder(20);
    buf.append(code.isBuiltIn() ? "built-in " : "user ");
    buf.append(code.getCodeType());
    buf.append("s");
    return buf.toString();
  }

  // FIXME: should not work on string,
  //        should be in the documentation code in Compiler
  /**
   * Convert argument description for suggestion display.
   */
  public String convertArgsDescr(String args) {
    if (null == args) {
      return "";
    }
    args = args.replaceFirst("^\\[", "");
    args = args.replaceFirst("\\]$", "");
    if (args.contains("&REST, ")) {
      args = args.replace("&REST, ", "");
      args = args + "...";
    }
    // FIXME: keys
    return args;
  }


  /**
   * Return suggestions for function calls.
   */
  public List<Suggestion> mkFunctionsSuggestions(Compiler.ICtx ctx) {
    final List<Suggestion> results = list();
    final Compiler compiler = ctx.getCompiler();
    for (String fun : compiler.getFunKeys()) {
      ICode code = compiler.getFun(fun);
      String args = convertArgsDescr(code.getArgDescr());
      Suggestion s =
          new Suggestion(
              fun + "(",
              codeType(code),
              "(", // suffix
              map(
                  DISPLAYNAME_KEY,
                  (fun + "(" + args + ")"),
                  DESCRIPTION_KEY,
                  docstringSummary(code.getDocstring())));
      results.add(s);
    }
    return results;
  }

  /**
   * Convert Map of variable properties for display.
   */
  public Map<String, String> convertVariableProps(Map<Object, Object> props) {
    Map<String, String> results = map();
    if (null != props) {
      for (Map.Entry<Object, Object> e : props.entrySet()) {
        results.put("" + e.getKey(), "" + e.getValue());
      }
    }
    return results;
  }

  /**
   * Make list of suggestions for a variable.
   */
  public List<Suggestion> mkVariablesSuggestions(Compiler.ICtx ctx) {
    final List<Suggestion> results = list();
    Map<String, Object> matches = ctx.findMatches("");
    for (String m : matches.keySet()) {
      Suggestion s = new Suggestion(m, "variable", convertVariableProps(ctx.getPropsMap().get(m)));
      results.add(s);
    }
    return results;
  }

  /**
   * Return list of suggestions for a symbol literal.
   */
  public List<Suggestion> mkSymbolSuggestions(Compiler.ICtx ctx) {
    final List<Suggestion> results = list();
    // FIXME: allow matching according pefix
    //        for web usage
    results.addAll(mkFunctionsSuggestions(ctx));
    results.addAll(mkVariablesSuggestions(ctx));
    // for (String m : ctx.getCompiler().getFunKeys()
    return results;
  }

  /**
   * Return list of suggestions for operators.
   */
  public List<Suggestion> mkOperatorSuggestions(Compiler.ICtx ctx, Integer tokType) {
    final List<Suggestion> results = list();
    String lit = unquote(AlgParserParser.VOCABULARY.getLiteralName(tokType));
    // final String sym = unquote(AlgParserParser.VOCABULARY.getSymbolicName(tokType));
    // final String dsp = unquote(AlgParserParser.VOCABULARY.getDisplayName(tokType));
    if (null == lit) {
      lit = operatorTokenTypes.get(tokType);
    }
    final ICode code = ctx.getCompiler().getFun(lit);
    String descr =
        null != code
            ? docstringSummary(code.getDocstring())
            : (null != tokenDescrs.get(tokType) ? tokenDescrs.get(tokType) : "operator " + lit);
    Suggestion s =
        new Suggestion(
            lit,
            "operators",
            map("literalName", lit, DESCRIPTION_KEY, descr, DISPLAYNAME_KEY, lit));
    results.add(s);
    return results;
  }

  // FIXME: it gets ugly and long, need global table or put this metadata into grammar somehow
  // FIXME: UI wise should distinguish between function calls, variables, :keywords
  /**
   * Return kind of token for the autosuggestion mechanism given ANTLR token type.
   */
  public String getTokenKind(int tokType) {
    if (AlgParserParser.SYMBOL == tokType /*||
                                            AlgParserParser.KEYWORD == tokType*/) {
      return TOKKIND_SYMBOL;
    } else if (operatorTokenTypes.containsKey(tokType)) {
      return TOKKIND_OPERATOR;
    } else if (AlgParserParser.NUMBER == tokType) {
      return TOKKIND_NUMBER;
    } else if (AlgParserParser.NIL_LIT == tokType) {
      return TOKKIND_NIL;
    } else if (AlgParserParser.TRUE_LIT == tokType || AlgParserParser.FALSE_LIT == tokType) {
      return TOKKIND_BOOLEAN;
    } else if (AlgParserParser.LP == tokType || AlgParserParser.RP == tokType) {
      return TOKKIND_PARENTHESES;
    } else if (AlgParserParser.BLOCK_COMMENT == tokType
        || AlgParserParser.LINE_COMMENT == tokType) {
      return TOKKIND_COMMENTS;
    } else if (AlgParserParser.WS == tokType) {
      return TOKKIND_WHITESPACE;
    } else {
      // FIXME: probably this sould not include things like comma, semicolon and so on
      return TOKKIND_KEYWORDS;
    }
  }

  /**
   * Build list of suggestions for a token.
   */
  public List<Suggestion> mkSuggestionForToken(
      Integer tokType, List<Integer> tokens, Compiler.ICtx ctx, boolean filter, Tokenization tz) {
    List<Suggestion> suggs;
    if (null == tokType) {
      return list();
    }
    if (AlgParserParser.SYMBOL == tokType) {
      suggs = mkSymbolSuggestions(ctx);
    } else if (operatorTokenTypes.containsKey(tokType)) {
      suggs = mkOperatorSuggestions(ctx, tokType);
    } else {
      suggs = list();
      final String lit = unquote(AlgParserParser.VOCABULARY.getLiteralName(tokType));
      final String sym = unquote(AlgParserParser.VOCABULARY.getSymbolicName(tokType));
      final String disp = unquote(AlgParserParser.VOCABULARY.getDisplayName(tokType));
      final String name = coalesce(lit, sym, disp);
      String descr = tokenDescrs.get(tokType);
      Suggestion s =
          new Suggestion(
              name,
              getTokenKind(tokType),
              map(
                  "literalName",
                  lit,
                  DESCRIPTION_KEY,
                  null != descr ? descr : "token '" + lit + "'",
                  DISPLAYNAME_KEY,
                  disp));
      suggs.add(s);
    }
    // FIXME: filter everywhere before adding to the list in the first place
    if (filter) {
      List<Suggestion> results = list();
      if (null != tz.tokenIndex) {
        final LanguageToken t = tz.tokens.get(tz.tokenIndex);
        // FIXME: check token kind
        // if (t.text.trim().length() == 0) {
        if (TOKKIND_WHITESPACE.equals(t.type)) {
          results.addAll(suggs);
        } else {
          for (Suggestion s : suggs) {
            if (s.text.startsWith(t.text)) {
              results.add(s);
            }
          }
        }
      }
      return results;
    } else {
      return suggs;
    }
  }

  /**
   * Check if token has given position.
   *
   * @param tok token
   * @param pos position
   * @param rightInclude include token that start at the cursor position.
   */
  public Token tokHasPos(Token tok, int pos, boolean rightInclude) {
    final int start = tok.getCharPositionInLine();
    final int stop = tok.getCharPositionInLine() + tok.getText().length();
    return (pos >= start && ((pos < stop) || (rightInclude && pos <= stop))) ? tok : null;
  }

  /**
   * Return token that has given cursor position.
   */
  public Token tokenHasPos(List<Token> tl, int pos) {
    if (null != tl) {
      for (Token t : tl) {
        if (null != tokHasPos(t, pos, false)) {
          return t;
        }
      }
    }
    return null;
  }

  /**
   * Compute index of current token for given cursor position.
   */
  public Integer computeTokenIndex(ParseTree tree, CommonTokenStream ts, int pos) {
    Integer result = null;
    if (tree instanceof TerminalNode) {
      final TerminalNode t = (TerminalNode) tree;
      final Token termTok = t.getSymbol();
      final int tokIdx = termTok.getTokenIndex();
      Token tok = null;
      if ((null != (tok = tokHasPos(termTok, pos, false)))
          || (null != (tok = tokenHasPos(ts.getHiddenTokensToLeft(tokIdx), pos)))
          || (null != (tok = tokenHasPos(ts.getHiddenTokensToRight(tokIdx), pos)))) {
        result = tok.getTokenIndex();
      } else if (null != (tok = tokHasPos(termTok, pos, true))) {
        result = tok.getTokenIndex();
        // System.err.println("Right match tokenIdx="+result);
      }
    } else if (tree instanceof RuleNode) {
      // rule , error nodes?
      int childCount = tree.getChildCount();
      for (int i = 0; i < childCount; i++) {
        if (null != (result = computeTokenIndex(tree.getChild(i), ts, pos))) {
          break;
        }
      }
    } else {
      throw new RuntimeException("Unknown node type for " + tree.getClass());
    }
    return result;
  }

  protected static ParseCtx mkPctx(ParseCtx old, SyntaxError se) {
    ParseCtx pctx = old.clone();
    if (null != se) {
      pctx.setLine(se.line);
      pctx.setPos(se.charPositionInLine);
      // FIXME: get real offset, SyntaxError does not keep it
      pctx.setOff(-1);
    }
    return pctx;
  }

  protected static ParserException se2ParserException(ParseCtx oldPctx, SyntaxError se) {
    final ParseCtx pctx = mkPctx(oldPctx, se);
    return new ParserException(pctx, se.msg);
  }

  public class ExprVisitor extends AlgParserBaseVisitor<Object> {
    protected String srcName;

    public ExprVisitor(String srcName) {
      this.srcName = srcName;
    }

    // CATCH ( SYMBOL | '(' clzspec SYMBOL ')' ) block
    @Override
    public Object visitCatchspec(AlgParserParser.CatchspecContext ctx) {
      final ParseCtx pctx = makePctx(ctx);
      final String varName = ctx.SYMBOL().getText();
      final ASTN exASTN =  ctx.clzspec() == null
          ? new ASTNLeaf(symbol(java.lang.Exception.class.getCanonicalName()), pctx)
          : (ASTN) visit(ctx.clzspec());
      ASTNList catchASTN = new ASTNList(list(new ASTNLeaf(symbol("CATCH"), pctx),
                                             exASTN,
                                             new ASTNLeaf(symbol(varName), pctx)), pctx);
      final ASTNList catchBlock = (ASTNList) visit(ctx.block());
      catchASTN.addAll(catchBlock);
      return catchASTN;
    }

    //SYMBOL (. SYMBOL )*
    @Override
    public Object visitClzspec(AlgParserParser.ClzspecContext ctx) {
      StringBuilder buf = new StringBuilder();
      for (TerminalNode part : ctx.SYMBOL()) {
        if (buf.length() > 0) {
          buf.append('.');
        }
        buf.append(part.getText());
      }
      return new ASTNLeaf(symbol(buf.toString()), makePctx(ctx));
    }

    // TRY block catchspec* (FINALLY block)?  EB
    @Override
    public Object visitTry_expr(AlgParserParser.Try_exprContext ctx) {
      final ParseCtx pctx = makePctx(ctx);
      ASTNList result = new ASTNList(list(new ASTNLeaf(symbol("TRY"), pctx)), pctx);
      ASTNList tryBlock = (ASTNList) visit(ctx.block().get(0));
      result.addAll(tryBlock);
      if (null != ctx.catchspec()) {
        for (int idx = 0; idx < ctx.catchspec().size(); idx++) {
          final ASTN catchASTN = (ASTN) visit(ctx.catchspec(idx));
          result.add(catchASTN);
        }
      }
      if (null != ctx.FINALLY()) {
        // FIXME
        final ParseCtx finallyPctx = pctx;
        final int idx = ctx.block().size() - 1;
        ASTNList finallyBlock = (ASTNList) visit(ctx.block().get(idx));
        ASTNList finallyASTN =
            new ASTNList(list(new ASTNLeaf(symbol("FINALLY"), finallyPctx)), finallyPctx);
        finallyASTN.addAll(finallyBlock);
        result.add(finallyASTN);
      }
      return result;
    }

    // 'IF' expr block ('ELSEIF' expr block)*  ( 'ELSE' block )? 'END';
    @Override
    public Object visitIf_expr(AlgParserParser.If_exprContext ctx) {
      ParseCtx pctx = makePctx(ctx);
      ASTNList result = new ASTNList(list(new ASTNLeaf(symbol("COND"), pctx)), pctx);
      int blockCount = ctx.block().size();
      int exprCount = ctx.expr().size();
      for (int i = 0; i < blockCount; i++) {
        ASTN expr = i < exprCount ? (ASTN) visit(ctx.expr().get(i)) : new ASTNLeaf(true, pctx);
        ASTNList block = (ASTNList) visit(ctx.block().get(i));
        ASTNList part = new ASTNList(list(expr), pctx);
        part.addAll(block);
        result.add(part);
      }
      return result;
    }

    // expr as var ( | expr )+
    @Override
    public Object visitTh_at_expr(AlgParserParser.Th_at_exprContext ctx) {
      final ASTN e = (ASTN) visit(ctx.expr());
      return e;
    }

    @Override
    public Object visitTh_auto_expr(AlgParserParser.Th_auto_exprContext ctx) {
      final ParseCtx pctx = makePctx(ctx);
      if (null != ctx.vector() && ctx.vector().size() > 0) {
        return transAssocLookups(ctx.expr(0), ctx.vector(), pctx);
      }
      final ASTN startExpr = (ASTN) visit(ctx.expr(0));
      final ASTNList result =
          new ASTNList(list(new ASTNLeaf(symbol("@->"), pctx), startExpr), pctx);
      // for (; idx < ctx.getChildCount(); idx+=2) {
      ASTNList subexprs = null;

      ASTN expr = (ASTN) visit(ctx.expr(1));
      if (expr instanceof ASTNList) {
        ASTNList exprList = (ASTNList) expr;
        if (exprList.size() > 0) {
          ASTN first = exprList.get(0);
          if (!first.isList() && symbol("@->").equals(first.getObject())) {
            // ASTN newExpr = new ASTNList(list(), expr.getPctx());
            subexprs = exprList.subList(1, exprList.size());
          }
        }
      }
      if (null != subexprs) {
        result.addAll(subexprs);
      } else {
        result.add(expr);
      }
      return result;
    }

    // expr (as var)? (| expr )+
    // it works somewhat non-trivially:
    // the operator is right-associative, so originally
    // it gets parsed as
    // (A | (B | ...X))
    // and then recursively flattened into
    // (-> A B .. X)
    @Override
    public Object visitTh_as_expr(AlgParserParser.Th_as_exprContext ctx) {
      final ParseCtx pctx = makePctx(ctx);
      final ASTN startExpr = (ASTN) visit(ctx.expr(0));
      final String varName = ctx.SYMBOL().getText();
      final ASTNLeaf var = new ASTNLeaf(symbol(varName), pctx);
      ASTNList result =
          new ASTNList(list(new ASTNLeaf(symbol("AS->"), pctx), startExpr, var), pctx);
      ASTNList subexprs = null;
      ASTN expr = (ASTN) visit(ctx.expr(1));
      if (expr instanceof ASTNList) {
        ASTNList exprList = (ASTNList) expr;
        if (exprList.size() > 0) {
          ASTN first = exprList.get(0);
          if (!first.isList() && symbol("@->").equals(first.getObject())) {
            // ASTN newExpr = new ASTNList(list(), expr.getPctx());
            subexprs = exprList.subList(1, exprList.size());
          }
        }
      }
      if (null != subexprs) {
        result.addAll(subexprs);
      } else {
        result.add(expr);
      }
      return result;
    }

    @Override
    public Object visitWhile_expr(AlgParserParser.While_exprContext ctx) {
      final ParseCtx pctx = makePctx(ctx);
      final ASTN expr = (ASTN)visit(ctx.expr());
      final ASTNList blocks = new ASTNList(list(new ASTNLeaf(symbol("PROGN"), pctx)), pctx);
      blocks.addAll((ASTNList)visit(ctx.block()));
      final ASTNList result = new ASTNList(list(new ASTNLeaf(symbol("WHILE"), pctx),
                                                expr,
                                                blocks), pctx);
      return result;
    }

    // short lambda notation with one argument
    // symbol '->' expr
    @Override
    public Object visitMonolambda_expr(AlgParserParser.Monolambda_exprContext ctx) {
      final ParseCtx pctx = makePctx(ctx);
      final String argname = ctx.SYMBOL().getText();
      final ASTN expr = (ASTN) visit(ctx.expr());
      final ASTNList result = new ASTNList(list(new ASTNLeaf(symbol("LAMBDA"), pctx),
          new ASTNList(list(new ASTNLeaf(symbol(argname), pctx)), pctx), expr), makePctx(ctx));
      return result;
    }

    // short lambda notation
    //  '('  exprList?  ')' '->' expr
    @Override
    public Object visitSlambda_expr(AlgParserParser.Slambda_exprContext ctx) {
      ParseCtx pctx = makePctx(ctx);
      AlgParserParser.ExprListContext exprList = ctx.exprList();
      ASTN arglist = null == exprList ? null : (ASTN) visit(exprList);
      ASTN expr = (ASTN) visit(ctx.expr());
      ASTNList result = new ASTNList(list(new ASTNLeaf(symbol("LAMBDA"), pctx)),
                                     makePctx(ctx));
      if (null != arglist) {
        result.add(arglist);
      } else {
        result.add(new ASTNList(list(), makePctx(ctx)));
      }
      result.add(expr);
      return result;
    }

    protected ASTN transArg(ASTN arg) {
      ASTN result = null;
      if (isSETF(arg)) {
        final ASTNList argL = (ASTNList)arg;
        if (argL.get(2).isList() || null != argL.get(2).getObject()) {
          final ASTNList argWithInit = new ASTNList(list(argL.get(1)),argL.getPctx());
          argWithInit.add(argL.get(2));
          result = argWithInit;
        } else {
          result = argL.get(1);
        }
      } else {
        result = arg;
      }
      return result;
    }



    // lambda   : 'FUNCTION' SYMBOL? '(' exprList? ... ( ';' exprList? ... )?  ')' block 'END'
    //                                  ^args      ^rest     ^kw args  ^other keys
    @Override
    public Object visitLambda(AlgParserParser.LambdaContext ctx) {
      ParseCtx pctx = makePctx(ctx);

      final String symText = null == ctx.SYMBOL() ? null : ctx.SYMBOL().getText();
      final Symbol sym = symbol(symText);

      // regular args
      ASTNList posArgs = null == ctx.posargs ? null : (ASTNList) visit(ctx.posargs);

      boolean hasRest = null != ctx.rest;
      boolean hasKWRest = null != ctx.okeys;
      ASTNList result = new ASTNList(list(), makePctx(ctx));

      if (null != sym) {
        result.add(new ASTNLeaf(symbol("DEFUN"), pctx));
        result.add(new ASTNLeaf(sym, pctx));
      } else {
        result.add(new ASTNLeaf(symbol("LAMBDA"), pctx));
      }

      if (hasRest && hasKWRest) {
        throw new RuntimeException("Cannot have both regular and key-value varargs");
      }
      ASTNList trArgList = new ASTNList(list(),
                                    null == posArgs ? makePctx(ctx) : posArgs.getPctx());
      boolean isOptional = false;
      if (null != posArgs) {
        for (int i = 0; i < posArgs.size() - (hasRest ? 1 : 0); i++) {
          ASTN arg = posArgs.get(i);
          boolean isSETF = isSETF(arg);
          if (isSETF && !isOptional) {
            trArgList.add(new ASTNLeaf(new Symbol(ArgSpec.ARG_OPTIONAL), arg.getPctx()));
            isOptional = true;
          }
          trArgList.add(transArg(arg));
        }
      }
      ASTNList kwargs  = null == ctx.kwargs ? null : (ASTNList) visit(ctx.kwargs);
      if (null != kwargs) {
        if (hasKWRest) {
          trArgList.add(new ASTNLeaf(new Symbol(ArgSpec.ARG_REST), kwargs.getPctx()));
        }
        trArgList.add(new ASTNLeaf(new Symbol(ArgSpec.ARG_KEY), kwargs.getPctx()));
        if (hasKWRest) {
          trArgList.add(transArg(kwargs.get(kwargs.size() - 1)));
        }
        for (int i = 0; i < kwargs.size() - (hasKWRest ? 1 : 0); i++) {
          trArgList.add(transArg(kwargs.get(i)));
        }
        if (hasKWRest) {
          trArgList.add(new ASTNLeaf(new Symbol(ArgSpec.ARG_ALLOW_OTHER_KEYS), kwargs.getPctx()));
        }
      }
      if (hasRest) {
        ASTN arg = posArgs.get(posArgs.size() - 1);
        trArgList.add(new ASTNLeaf(new Symbol(ArgSpec.ARG_REST), arg.getPctx()));
        trArgList.add(transArg(arg));
      }
      result.add(trArgList);
      ASTNList block = (ASTNList) visit(ctx.block());
      result.addAll(block);
      return result;
    }


    @Override
    public Object visitReturn_expr(AlgParserParser.Return_exprContext ctx) {
      final ParseCtx pctx = makePctx(ctx);
      ASTNList result = new ASTNList(list(new ASTNLeaf(symbol("RETURN"), pctx)), pctx);
      // AlgParserParser.ExprListContext exprList =  ctx.exprList();
      AlgParserParser.ExprContext expr = ctx.expr();
      if (null != expr) {
        ASTN arg = (ASTN) visit(expr);
        result.add(arg);
      }
      return result;
    }


    @Override
    public Object visitFor_expr(AlgParserParser.For_exprContext ctx) {
      final ParseCtx pctx = makePctx(ctx);

      // AlgParserParser.ExprListContext exprList =  ctx.exprList();
      final AlgParserParser.ExprContext vars = ctx.loopfor().expr(0);
      final AlgParserParser.ExprContext vals = ctx.loopfor().expr(1);
      final AlgParserParser.ExprContext ret = ctx.loopfor().expr(2);
      final ASTN varsASTN = (ASTN) visit(vars);
      final ASTN valsASTN = (ASTN) visit(vals);
      final ASTN retASTN = null == ret ? null : (ASTN) visit(ret);
      final ASTNList block = (ASTNList)visit(ctx.loopfor().block());

      final List<ASTN>  args = list(varsASTN, valsASTN);
      if (null != retASTN) {
        args.add(retASTN);
      }
      final ASTNList result = new ASTNList(list(new ASTNLeaf(symbol("FOREACH"), pctx),
                                                new ASTNList(args, pctx)), pctx);
      result.addAll(block);
      return result;
    }


    @Override
    public Object visitVector_expr(AlgParserParser.Vector_exprContext ctx) {
      final ParseCtx pctx = makePctx(ctx);
      ASTNList result = new ASTNList(list(), pctx);
      result.setIsLiteralList(true);
      // AlgParserParser.ExprListContext exprList =  ctx.exprList();
      AlgParserParser.ExprListContext exprList = ctx.vector().exprList();
      if (null != exprList) {
        ASTNList arglist = (ASTNList) visit(exprList);
        result.addAll(arglist);
      }
      return result;
    }

    /** Parse and transpile dictionary definition (JSON like syntax { x : y, ...}). */
    @Override
    public Object visitDict_expr(AlgParserParser.Dict_exprContext ctx) {
      final ParseCtx pctx = makePctx(ctx);
      final int exprCount = ctx.expr().size();
      ASTNList result = new ASTNList(list(new ASTNLeaf(symbol("HASHMAP"), pctx)), pctx);
      for (int i = 0; i < exprCount; i += 2) {
        result.add((ASTN) visit(ctx.expr(i)));
        result.add((ASTN) visit(ctx.expr(i + 1)));
      }
      return result;
    }

    /** Parse and transpile quoted expression (:expr). */
    @Override
    public Object visitQuote_expr(AlgParserParser.Quote_exprContext ctx) {
      final ParseCtx pctx = makePctx(ctx);
      final ASTNList result =
          new ASTNList(list(new ASTNLeaf(symbol("QUOTE"), pctx), visit(ctx.expr())), pctx);
      return result;
    }

    /** Parse and transpile static association lookup expression (.). */
    @Override
    public Object visitDotchain(AlgParserParser.DotchainContext ctx) {
      final int childNum = ctx.getChildCount();
      final ParseCtx pctx = makePctx(ctx);
      final ASTNList indices = new ASTNList(list(new ASTNLeaf(symbol("LIST"), pctx)), pctx);
      final ASTN obj = (ASTN) visit(ctx.getChild(0));
      for (int i = 2; i < childNum; i += 2) {
        String idx = ctx.getChild(i).getText();
        indices.add(new ASTNLeaf(idx, pctx));
      }
      ASTNList result =
          new ASTNList(list(new ASTNLeaf(symbol("GET-IN"), pctx), obj, indices), pctx);
      return result;
    }

    /** Parse and transpile association lookup expression [key]. */
    private void parseLookup(List out, List<AlgParserParser.VectorContext> vectors, int i) {
      if (i >= vectors.size()) {
        return;
      }
      AlgParserParser.VectorContext vx = vectors.get(i);
      int vsize = null != vx.exprList() ? 1 : 0;
      if (vsize == 0) {
        List getexpr = list();
        getexpr.addAll(out);
        out.clear();
        List getin = list(symbol("GET-IN"), symbol("_"), list(symbol("LIST")));
        parseLookup(getin, vectors, i + 1);
        List lambda = list(symbol("LAMBDA"), list(symbol("_")), getin);
        out.addAll(list(symbol("MAP"), lambda, getexpr));
      } else {
        final ASTN idx = (ASTN) visit(vx.exprList().expr(0));
        List indices = (List) out.get(2);
        indices.add(idx);
        parseLookup(out, vectors, i + 1);
      }
    }

    private Object transAssocLookups(
        AlgParserParser.ExprContext expr,
        List<AlgParserParser.VectorContext> vectors,
        ParseCtx pctx) {
      final ASTN obj = (ASTN) visit(expr);
      final List indices = list(symbol("LIST"));
      List result = list(symbol("GET-IN"), obj, indices);
      parseLookup(result, vectors, 0);
      return astnize(result, pctx);
    }

    @Override
    public Object visitAssoc_lookup(AlgParserParser.Assoc_lookupContext ctx) {
      return transAssocLookups(ctx.expr(), ctx.vector(), makePctx(ctx));
    }

    // expr  DWIM_MATCHES expr                         # dwim_matches_expr
    @Override
    public Object visitDwim_matches_expr(AlgParserParser.Dwim_matches_exprContext ctx) {
      ParseCtx pctx = makePctx(ctx);
      final ASTN obj = (ASTN) visit(ctx.getChild(0));
      final ASTN pat = (ASTN) visit(ctx.getChild(2));
      ASTNList result =
          new ASTNList(list(new ASTNLeaf(symbol("DWIM_MATCHES"), pctx), obj, pat), pctx);
      return result;
    }

    // expr  DWIM_MATCHES expr                         # dwim_matches_expr
    @Override
    public Object visitDwim_search_expr(AlgParserParser.Dwim_search_exprContext ctx) {
      ParseCtx pctx = makePctx(ctx);
      final ASTN obj = (ASTN) visit(ctx.getChild(0));
      final ASTN pat = (ASTN) visit(ctx.getChild(2));
      ASTNList result =
          new ASTNList(list(new ASTNLeaf(symbol("SEARCH"), pctx), obj, pat), pctx);
      return result;
    }

    @Override
    public Object visitFspart(AlgParserParser.FspartContext ctx) {
      final ParseCtx pctx = makePctx(ctx);
      if (null != ctx.STRING()) {
        String[] holder = new String[1];
        if (! escStringParser.parse(ctx.STRING().getText(), holder, pctx)) {
          throw new RuntimeException("Internal error: failed to parse string in fspart: '"
                                     + ctx.STRING() + "'");
        }
        return new ASTNLeaf(holder[0], pctx);
      }
      if (null != ctx.SYMBOL()) {
        return new ASTNLeaf(ctx.SYMBOL().getText(), pctx);
      }
      throw new RuntimeException("Internal error: failed to parse fspart: "
                                 + "expect SYMBOL or STRING token, but got none");
    }

    //  fspart ('.' fspart)*  ('(' fieldspec (',' fieldspec)* ')')?  (ASOP fspart)?
    @Override
    public Object visitFieldspec(AlgParserParser.FieldspecContext ctx) {
      final ParseCtx pctx = makePctx(ctx);
      ASTN dstASTN = null;
      if (ctx.ASOP() != null) {
        AlgParserParser.FspartContext dst = ctx.fspart(ctx.fspart().size() - 1);
        dstASTN = (ASTN) visit(dst);
      }
      final ASTNList src = new ASTNList(list(new ASTNLeaf(symbol("LIST"),pctx)), pctx);

      ASTNList subspecs = null;
      if (ctx.fieldspec().size() > 0) {
        subspecs = new ASTNList(list(new ASTNLeaf(symbol("LIST"),makePctx(ctx))), pctx);
        for (int specIdx = 0; specIdx < ctx.fieldspec().size(); specIdx++) {
          subspecs.add((ASTN) visit(ctx.fieldspec(specIdx)));
        }
      }

      ASTN srcPartASTN = null;
      int numSrcParts = (ctx.fspart().size() - (null == dstASTN ? 0 : 1));
      for (int partIdx = 0; partIdx < numSrcParts; partIdx++) {
        AlgParserParser.FspartContext srcPart = ctx.fspart(partIdx);
        srcPartASTN = (ASTN)visit(srcPart);
        src.add(srcPartASTN);
      }
      if (null == dstASTN && (numSrcParts == 1) && subspecs == null) {
        return srcPartASTN;
      }
      if (null == dstASTN) {
        if (null == subspecs) {
          // if no destination use last part of source as destination key
          dstASTN = srcPartASTN;
        } else {
          dstASTN = new ASTNLeaf(null, makePctx(ctx));
        }
      }

      ASTNList specASTN = new ASTNList(list(new ASTNLeaf(symbol("LIST"), pctx)), pctx);

      specASTN.add((numSrcParts == 1) ? srcPartASTN : src);
      specASTN.add(dstASTN);
      if (null != subspecs) {
        specASTN.add(subspecs);
      }
      return specASTN;

    }

    // expr  FIELDSOP  fieldspec (',' fieldspec)*
    @Override
    public Object visitFields_expr(AlgParserParser.Fields_exprContext ctx) {
      final ParseCtx pctx = makePctx(ctx);
      final ASTN obj = (ASTN) visit(ctx.expr());

      final int specsSize = ctx.fieldspec().size();
      final ASTNList ks = new ASTNList(list(new ASTNLeaf(symbol("LIST"), pctx)), pctx);
      for (int specIdx = 0; specIdx < specsSize; specIdx++) {
        ks.add((ASTN) visit(ctx.fieldspec(specIdx)));
      }
      ASTNList result = new ASTNList(list(new ASTNLeaf(symbol("DWIM_FIELDS"), pctx),
                                          obj,
                                          ks), pctx);
      return result;
    }

    // exprList : expr (',' expr)*
    @Override
    public Object visitExprList(AlgParserParser.ExprListContext ctx) {
      // System.out.println("visit exprList");
      ASTNList result = new ASTNList(list(), makePctx(ctx));
      final int numKids = ctx.getChildCount();
      for (int i = 0; i < numKids; i += 2) {
        Object childASTN = visit(ctx.getChild(i));
        // System.out.println("visit ["+i+"] = " +
        // ctx.getChild(i).getText()
        // + "->" + childASTN);
        result.add((ASTN) childASTN);
      }
      return result;
    }

    // expr LASSIGN expr
    @Override
    public Object visitAssign_expr(AlgParserParser.Assign_exprContext ctx) {
      ParseCtx pctx = makePctx(ctx);
      final ASTN var = (ASTN) visit(ctx.expr(0));
      final ASTN val = (ASTN) visit(ctx.expr(1));
      final ASTNList result =
          new ASTNList(list(new ASTNLeaf(symbol("SETF"), pctx), var, val), pctx);
      return result;
    }

    @Override
    public Object visitOptassign(AlgParserParser.OptassignContext ctx) {
      ParseCtx pctx = makePctx(ctx);
      ASTNList result =
          new ASTNList(list(new ASTNLeaf(symbol(ctx.SYMBOL().getText()), pctx)), pctx);
      if (null != ctx.expr()) {
        final ASTN val = (ASTN) visit(ctx.expr());
        result.add(val);
      }
      return result;
    }
    
    
    // LET (SYMBOL LASSIGN expr (',' SYMBOL LASSIGN  expr)*)? block EB # let_expr
    @Override
    public Object visitLet_expr(AlgParserParser.Let_exprContext ctx) {
      ParseCtx pctx = makePctx(ctx);
      final ASTNList result = new ASTNList(list(new ASTNLeaf(symbol("LET"), pctx)), pctx);
      final int numVars = ctx.optassign().size();
      
      final ASTNList vars = new ASTNList(list(), pctx);
      for (int idx = 0; idx < numVars; idx++) {
        //final ASTN val = (ASTN) visit(ctx.expr(idx));
        vars.add((ASTN) visit(ctx.optassign(idx)));
        //    new ASTNList(list(new ASTNLeaf(symbol(ctx.SYMBOL(idx).getText()), pctx), val), pctx));
      }
      result.add(vars);
      result.addAll((ASTNList) visit(ctx.block()));
      return result;
    }

    // (LOCAL|GLOBAL)? SYMBOL LASSIGN expr
    @Override
    public Object visitVardecl_expr(AlgParserParser.Vardecl_exprContext ctx) {
      ParseCtx pctx = makePctx(ctx);
      final String op = ctx.mod == null ? null : ctx.mod.getText();
      String setx = "SETV";
      if ("global".equalsIgnoreCase(op)) {
        setx = "SETQ";
      } else if ("local".equalsIgnoreCase(op)) {
        setx = "SETL";
      }
      final ASTN var = new ASTNLeaf(symbol(ctx.sym.getText()), pctx);
      final ASTN val = (ASTN) visit(ctx.expr());
      final ASTNList result =
          new ASTNList(list(new ASTNLeaf(symbol(setx), pctx), var, val), pctx);
      return result;
    }


    @Override
    public Object visitChar_expr(AlgParserParser.Char_exprContext ctx) {
      final ParseCtx pctx = makePctx(ctx);
      final String strVal = ctx.getText();
      char c = strVal.charAt(1);
      if (c == '\\') {
        c = strVal.charAt(2);
        switch (c) {
          case 'b':
            c = '\b';
            break;
          case 't':
            c = '\t';
            break;
          case 'n':
            c = '\n';
            break;
          case 'f':
            c = '\f';
            break;
          case 'r':
            c = '\r';
            break;
          case 'u':
          case 'U':
            if (strVal.length() > 4) {
              final int i = Integer.parseInt(strVal.substring(3, strVal.length() - 1), 16);
              c = (char) i;
            }
            break;
          default:
            break;
        }
      }
      final ASTN astn = new ASTNLeaf(c, pctx);
      return astn;
    }


    @Override
    public Object visitAtom_expr(AlgParserParser.Atom_exprContext ctx) {
      final String strVal = ctx.getText();
      ASTN astn = parseAtom(strVal, makePctx(ctx));
      return astn;
    }

    //  expr  op=( MULOP | DIVOP | REMOP ) expr
    @Override
    public Object visitProduct_expr(AlgParserParser.Product_exprContext ctx) {
      final Object left = visit(ctx.expr(0));
      final Object right = visit(ctx.expr(1));
      Symbol opsym;
      final int opType = ctx.op.getType();
      if (opType == AlgParserParser.DIVOP) {
        opsym = symbol("/");
      } else if (opType == AlgParserParser.MULOP) {
        opsym = symbol("*");
      } else if (opType == AlgParserParser.REMOP) {
        opsym = symbol("%");
      } else {
        throw new RuntimeException("Internal error: unknown operator token " + ctx.op);
      }
      ASTN opASTN = new ASTNLeaf(opsym, makePctx(ctx));
      ASTN result = new ASTNList(list(opASTN, (ASTN) left, (ASTN) right), makePctx(ctx));
      return result;
    }

    // expr  op=( ADDOP | SUBOP ) expr
    @Override
    public Object visitSum_expr(AlgParserParser.Sum_exprContext ctx) {
      final Object left = visit(ctx.expr(0));
      final Object right = visit(ctx.expr(1));
      Symbol opsym;
      final int opType = ctx.op.getType();
      if (opType == AlgParserParser.ADDOP) {
        opsym = symbol("+");
      } else if (opType == AlgParserParser.SUBOP) {
        opsym = symbol("-");
      } else {
        throw new RuntimeException("Internal error: unknown operator token " + ctx.op);
      }
      ASTN opASTN = new ASTNLeaf(opsym, makePctx(ctx));
      ASTN result = new ASTNList(list(opASTN, (ASTN) left, (ASTN) right), makePctx(ctx));
      return result;
    }

    // expr  op=( NUMLT | NUMGT | NUMGE | NUMLE ) expr
    @Override
    public Object visitNumcomp_expr(AlgParserParser.Numcomp_exprContext ctx) {
      final Object left = visit(ctx.expr(0));
      final Object right = visit(ctx.expr(1));
      Symbol opsym;
      switch (ctx.op.getType()) {
        case (AlgParserParser.NUMGE):
          opsym = symbol(">=");
          break;
        case (AlgParserParser.NUMGT):
          opsym = symbol(">");
          break;
        case (AlgParserParser.NUMLE):
          opsym = symbol("<=");
          break;
        case (AlgParserParser.NUMLT):
          opsym = symbol("<");
          break;
        default:
          throw new RuntimeException("Internal error: unknown operator token " + ctx.op);
      }
      ASTN opASTN = new ASTNLeaf(opsym, makePctx(ctx));
      ASTN result = new ASTNList(list(opASTN, (ASTN) left, (ASTN) right), makePctx(ctx));
      return result;
    }

    // expr  op=( EQUAL | NOTEQUAL | NUMEQUAL) expr
    @Override
    public Object visitEquality_expr(AlgParserParser.Equality_exprContext ctx) {
      final Object left = visit(ctx.expr(0));
      final Object right = visit(ctx.expr(1));
      Symbol opsym;
      boolean inv = false;
      switch (ctx.op.getType()) {
        case (AlgParserParser.ISSAME):
          opsym = symbol("===");
          break;
        case (AlgParserParser.NUMEQUAL):
          opsym = symbol("=");
          break;
        case (AlgParserParser.EQUAL):
          opsym = symbol("==");
          break;
        case (AlgParserParser.NOTEQUAL):
          opsym = symbol("==");
          inv = true;
          break;
        case (AlgParserParser.NOTSAME):
          opsym = symbol("===");
          inv = true;
          break;
        default:
          throw new RuntimeException("Internal error: unexpected operator token " + ctx.op);
      }
      ParseCtx pctx = makePctx(ctx);
      ASTN result = new ASTNList(list(new ASTNLeaf(opsym, pctx), (ASTN) left, (ASTN) right), pctx);
      if (inv) {
        result = new ASTNList(list(new ASTNLeaf(symbol("NOT"), pctx), result), makePctx(ctx));
      }
      return result;
    }

    // block    : expr  (';' expr )* ';'?
    @Override
    public Object visitBlock(AlgParserParser.BlockContext ctx) {
      // System.out.println("visit block");
      ASTNList result = new ASTNList(list(), makePctx(ctx));
      final int numKids = ctx.getChildCount();
      for (int i = 0; i < numKids; i += 2) {
        Object childASTN = visit(ctx.getChild(i));
        // System.out.println("visit ["+i+"] = " +
        // ctx.getChild(i).getText()
        // + "->" + childASTN);
        result.add((ASTN) childASTN);
      }
      return result;
    }

    @Override
    public Object visitReplblock(AlgParserParser.ReplblockContext ctx) {
      // final ParseCtx pctx = makePctx(ctx);
      final Object expr = visit(ctx.block());
      return expr;
    }

    // beblock  : 'BEGIN' block 'END'
    @Override
    public Object visitBeblock(AlgParserParser.BeblockContext ctx) {
      // System.out.println("visit beblock");
      ParseCtx pctx = makePctx(ctx);
      ASTNList result = new ASTNList(list(new ASTNLeaf(symbol("PROGN"), pctx)), makePctx(ctx));
      ASTNList blockASTN = (ASTNList) visit(ctx.block());
      result.addAll(blockASTN);
      return result;
    }

    // expr  'AND'    expr
    @Override
    public Object visitAnd_expr(AlgParserParser.And_exprContext ctx) {
      final Object left = visit(ctx.expr(0));
      final Object right = visit(ctx.expr(1));
      ParseCtx pctx = makePctx(ctx);
      final ASTN opASTN = new ASTNLeaf(symbol("AND"), pctx);
      ASTN result = new ASTNList(list(opASTN, (ASTN) left, (ASTN) right), pctx);
      return result;
    }

    // expr  'OR'     expr
    @Override
    public Object visitOr_expr(AlgParserParser.Or_exprContext ctx) {
      final Object left = visit(ctx.expr(0));
      final Object right = visit(ctx.expr(1));
      ParseCtx pctx = makePctx(ctx);
      final ASTN opASTN = new ASTNLeaf(symbol("OR"), pctx);
      ASTN result = new ASTNList(list(opASTN, (ASTN) left, (ASTN) right), pctx);
      return result;
    }

    // expr  'OR'     expr
    @Override
    public Object visitIn_expr(AlgParserParser.In_exprContext ctx) {
      final Object left = visit(ctx.expr(0));
      final Object right = visit(ctx.expr(1));
      final ParseCtx pctx = makePctx(ctx);
      final String symName = ctx.NOTOP() == null ? "IN" : "NOT-IN";
      final ASTN opASTN = new ASTNLeaf(symbol(symName), pctx);
      ASTN result = new ASTNList(list(opASTN, (ASTN) left, (ASTN) right), pctx);
      return result;
    }

    // op=( SUBOP | ADDOP ) expr
    @Override
    public Object visitSign_expr(AlgParserParser.Sign_exprContext ctx) {
      final Object expr = visit(ctx.expr());
      final ParseCtx pctx = makePctx(ctx);
      switch (ctx.op.getType()) {
        case AlgParserParser.ADDOP:
          // need to retain it being function
          // because of threading
          // return expr;
          return new ASTNList(list(new ASTNLeaf(symbol("+"), pctx), expr), pctx);
        case AlgParserParser.SUBOP:
          return new ASTNList(list(new ASTNLeaf(symbol("-"), pctx), expr), pctx);
        default:
          throw new RuntimeException("Internal error: unknown operator token " + ctx.op);
      }
    }

    // 'NOT'                expr
    @Override
    public Object visitNot_expr(AlgParserParser.Not_exprContext ctx) {
      final ParseCtx pctx = makePctx(ctx);
      final Object expr = visit(ctx.expr());
      return new ASTNList(list(new ASTNLeaf(symbol("NOT"), pctx), expr), pctx);
    }

    // @Override
    // public Object visitSymfunc_expr(@NotNull AlgParserParser.Symfunc_exprContext ctx) {
    //     final ParseCtx pctx = makePctx(ctx);
    //     final String strVal = ctx.getText();
    //     if (strVal.length() < 4 ||
    //         !strVal.startsWith("f\"") ||
    //         !strVal.endsWith("\"")) {
    //         throw new RuntimeException("Invalid function literal: " + strVal +
    //                                    " expect f\"name\"");
    //     }
    //     String[] fname = new String[0];
    //     if (escStringParser.parse(strVal.substring(1), fname, pctx)) {
    //         return new ASTNList(list(new ASTNLeaf(symbol("FUNCTION"), pctx),
    //                                  new ASTNLeaf(symbol(fname[0]), pctx)), pctx);
    //     } else {
    //         throw new RuntimeException("Invalid function literal: " + strVal +
    //                                    " failed to get function name");
    //     }
    // }

    private String ipolUnescape(String str, int start, int end) {
      final StringBuilder sb = new StringBuilder(end - start);
      for (int i = start; i < end; i++) {
        char c = str.charAt(i);
        if (c == '\\') {
          i++;
          if (i < str.length()) {
            c = str.charAt(i);
            switch (c) {
              case 'b':
                c = '\b';
                break;
              case 't':
                c = '\t';
                break;
              case 'n':
                c = '\n';
                break;
              case 'f':
                c = '\f';
                break;
              case 'r':
                c = '\r';
                break;
              default:
                break;
            }
          } else {
            return null;
          }
        }
        sb.append(c);
      }
      return sb.toString();
    }

    @Override
    public Object visitIpol_expr(AlgParserParser.Ipol_exprContext ctx) {
      final ParseCtx pctx = makePctx(ctx);
      if (null != ctx.IPOL_VOID()) {
        final String voidStr = ctx.IPOL_VOID().getText();
        return new ASTNLeaf(ipolUnescape(voidStr, 2, voidStr.length() - 1), pctx);
      } else {
        final ASTNList result = new ASTNList(list(new ASTNLeaf(symbol("STR"), pctx)), pctx);
        // final Object expr = visit(ctx.expr());
        final String startStr = ctx.IPOL_START().getText();
        if (startStr.length() > 4) {
          result.add(new ASTNLeaf(ipolUnescape(startStr, 2, startStr.length() - 2), pctx));
        }

        result.add((ASTN) visit(ctx.expr(0)));
        int middleCount = ctx.IPOL_MIDDLE().size();
        for (int i = 0; i < middleCount; i++) {
          final String mStr = ctx.IPOL_MIDDLE(i).getText();
          if (mStr.length() > 3) {
            result.add(new ASTNLeaf(ipolUnescape(mStr, 1, mStr.length() - 2), pctx));
          }
          result.add((ASTN) visit(ctx.expr(i + 1)));
        }

        final String endStr = ctx.IPOL_END().getText();
        if (endStr.length() > 2) {
          result.add(new ASTNLeaf(ipolUnescape(endStr, 1, endStr.length() - 1), pctx));
        }
        return result;
      }
    }

    // LP expr RP
    @Override
    public Object visitParen_expr(AlgParserParser.Paren_exprContext ctx) {
      final Object expr = visit(ctx.expr());
      return expr;
    }

    // SYMBOL '('  exprList?  ')'
    @Override
    public Object visitFuncall_expr(AlgParserParser.Funcall_exprContext ctx) {
      final ParseCtx pctx = makePctx(ctx);
      int numKids = ctx.getChildCount();
      boolean hasExprList;
      if (numKids == 3) {
        hasExprList = false;
      } else if (numKids == 4) {
        hasExprList = true;
      } else {
        throw new RuntimeException(
            "Internal error: invalid numner of tokens when translating funcall");
      }
      ASTN callSym = new ASTNLeaf(symbol(ctx.getChild(0).getText()), pctx);
      ASTNList result = new ASTNList(list(callSym), pctx);
      if (hasExprList) {
        convertArgList(result, (ASTNList) visit(ctx.getChild(2)));
      }
      return result;
    }

    protected void convertArgList(ASTNList result, ASTNList args) {
      for (int i = 0; i < args.size(); i++) {
        ASTN arg = args.get(i);
        if (arg.isList()) {
          final ASTNList argLst = (ASTNList) arg;
          if (argLst.size() == 3
              && !argLst.get(0).isList()
              && symbol("SETF").equals(argLst.get(0).getObject())
              && !argLst.get(1).isList()
              && (argLst.get(1).getObject() instanceof Symbol)) {
            result.add(
                new ASTNLeaf(
                    new Keyword(":" + argLst.get(1).getObject()), argLst.get(1).getPctx()));
            result.add(argLst.get(2));
            continue;
          }
        }
        result.add(arg);
      }
    }

    // '(' lambda ')' '('  exprList?  ')'
    @Override
    public Object visitLambdacall_expr(AlgParserParser.Lambdacall_exprContext ctx) {
      final ParseCtx pctx = makePctx(ctx);
      int numKids = ctx.getChildCount();
      boolean hasExprList;
      if (numKids == 5) {
        hasExprList = false;
      } else if (numKids == 6) {
        hasExprList = true;
      } else {
        throw new RuntimeException(
            "Internal error: invalid numner of tokens when translating funcall");
      }
      ASTN lambda =  (ASTN)visit(ctx.getChild(1));
      ASTNList result = new ASTNList(list(lambda), pctx);
      if (hasExprList) {
        convertArgList(result, (ASTNList) visit(ctx.getChild(4)));
      }
      return result;
    }

    /** Make new Parse context on basis of ANTLR parsing information. */
    public ParseCtx makePctx(ParserRuleContext rctx) {
      // FIXME: offset
      final ParseCtx pctx =
          new ParseCtx(srcName,
                     rctx.start.getLine(),
                     rctx.start.getCharPositionInLine(),
                     -1,
                     rctx.getText().length());
      return pctx;
    }
  }


  private ASTN parseAtom(String string, ParseCtx pctx) {
    final Object[] holder = new Object[1];
    for (AtomParser parser : atomParsers) {
      try {
        if (parser.parse(string, holder, pctx)) {
          // return new ASTNLeaf(holder[0], pctx);
          return astnize(holder[0], pctx);
        }
      } catch (AtomParseException ex) {
        return new ASTNLeaf(string, pctx, ex);
      }
    }
    return new ASTNLeaf(
        string, pctx, new ParserException(String.format("Failed to parse atom '%s'", string)));
  }

  protected final EscStringParser escStringParser = new EscStringParser();
  protected AtomParser[] atomParsers =
      new AtomParser[] {
        new NullParser(),
        new BooleanParser(),
        new NumberParser(),
        escStringParser,
        new RegexpParser(),
        new SymfuncParser(),
        new VersionParser(),
        // new KeywordParser(),
        new SymbolParser()
      };

  /** Convert S-exp string representaion in informational/error messages. */
  public String sexpToString(Object obj) {
    if (null == obj) {
      return "()";
    } else if (obj instanceof List) {
      StringBuffer str = new StringBuffer();
      str.append('(');
      for (Object member : ((List) obj)) {
        if (str.length() > 1) {
          str.append(" ");
        }
        str.append(sexpToString(member));
      }
      str.append(')');
      return str.toString();
    } else if (obj instanceof String) {
      return String.format("\"%s\"", obj);
    } else {
      return obj.toString();
    }
  }

  protected void clearBuf(StringBuffer buf) {
    buf.delete(0, buf.length());
  }

  protected List list(Object... objs) {
    List lst = new ArrayList();
    lst.addAll(Arrays.asList(objs));
    return lst;
  }

  public static class SyntaxError {
    Recognizer recognizer;
    Object offendingSymbol;
    int line;
    int charPositionInLine;
    String msg;
    RecognitionException ex;

    /**
     *Create infomration record for syntax error.
     */
    public SyntaxError(
        Recognizer<?, ?> recognizer,
        Object offendingSymbol,
        int line,
        int charPositionInLine,
        String msg,
        RecognitionException ex) {
      this.recognizer = recognizer;
      this.line = line;
      this.charPositionInLine = charPositionInLine;
      this.offendingSymbol = offendingSymbol;
      this.msg = msg;
      this.ex = ex;
    }

    public String toString() {
      return "" + line + ":" + charPositionInLine + ": " + msg;
    }
  }

  public static class SyntaxErrorListener extends BaseErrorListener {
    private final List<SyntaxError> syntaxErrors = new ArrayList<>();

    SyntaxErrorListener() {
    }

    List<SyntaxError> getSyntaxErrors() {
      return syntaxErrors;
    }

    @Override
    public void syntaxError(
        Recognizer<?, ?> recognizer,
        Object offendingSymbol,
        int line,
        int charPositionInLine,
        String msg,
        RecognitionException e) {
      syntaxErrors.add(
          new SyntaxError(recognizer, offendingSymbol, line, charPositionInLine, msg, e));
    }

    @Override
    public String toString() {
      return Utils.join(syntaxErrors.iterator(), "\n");
    }
  }

  private static String formatModifiers(ArgSpec.Arg arg) {
    if (arg.isLazy() || arg.isPipe()) {
      return "#= "
        + (arg.isLazy()  ? "&LAZY " : "")
        + (arg.isPipe()  ? "&PIPE " : "")
        + " =# ";
    } else {
      return "";
    }
  }

  protected boolean isParen(String str) {
    return "(".equals(str) || ")".equals(str);
  }

  protected String argsJoin(List<String> args) {
    final StringBuilder buf = new StringBuilder();
    final int size = args.size();
    for (int i = 0; i < size; i++) {
      final String arg = args.get(i);
      buf.append(arg);
      if (i < size - 1 && (!("(".equals(arg) || ")".equals(args.get(i + 1))))) {
        buf.append(", ");
      }
    }
    return buf.toString();
  }

  @Override
  public String formatArgSpec(ArgSpec spec) {
    // FIXME: kluge
    StringBuilder buf = new StringBuilder(64);
    if (null != spec) {
      List<String> posargs = new ArrayList<String>();
      List<String> kwargs     = new ArrayList<String>();
      boolean otherKeys = false;
      for (int i = 0; i < spec.size(); i++) {
        ArgSpec.Arg arg = spec.getArg(i);
        ArgSpec.AF flag = arg.getFlag();
        if (ArgSpec.AF.KEY == flag
            || ArgSpec.AF.REST_KEY == flag) {
          if (arg.isAllowOtherKeys()) {
            otherKeys = true;
          }
          kwargs.add(isParen(arg.getName())
                     ? arg.getName()
                     : (formatModifiers(arg)
                        + arg.getName()
                        + ":=" + (null == arg.getInit() ? "NIL" : arg.getInit())));
        } else {
          posargs.add(isParen(arg.getName())
                      ? arg.getName()
                      : (formatModifiers(arg)
                         + arg.getName()
                         + ((flag == ArgSpec.AF.OPTIONAL)
                            ? ":=" + (null == arg.getInit() ? "NIL" : arg.getInit())
                            : "")
                         + ((flag == ArgSpec.AF.REST) ? "..." : "")));
        }
      }
      buf.append(argsJoin(posargs));
      if (kwargs.size() > 0) {
        buf.append("; ");
        buf.append(argsJoin(kwargs));
      }
      if (otherKeys) {
        buf.append("...");
      }
    } else {
      buf.append("args...");
    }
    return buf.toString();
  }

  private static boolean isSETF(ASTN arg) {
    if (arg.isList()) {
      final ASTNList argLst = (ASTNList) arg;
      if (argLst.size() == 3 && !argLst.get(0).isList()
          && symbol("SETF").equals(argLst.get(0).getObject()) && !argLst.get(1).isList()
          && (argLst.get(1).getObject() instanceof Symbol)) {
        return true;
      }
    }
    return false;
  }

  // FIXME: use rule labels somehow?
  private final Map<Integer, String> tokenDescrs =
      map(
          AlgParserParser.NIL_LIT, "NIL Literal",
          AlgParserParser.TRUE_LIT, "TRUE Boolean Literal",
          AlgParserParser.FALSE_LIT, "FALSE Boolean Literal",
          AlgParserParser.NUMBER, "Numeric Literal ",
          AlgParserParser.STRING, "\"String Literal\"",
          // AlgParserParser.KEYWORD, ":KEYWORD",
          AlgParserParser.LP, "Open Subexpression/Arglist",
          AlgParserParser.RP, "Close Subexpression/Arglist",
          AlgParserParser.NUMEQUAL, "Test Numeric Equality",
          AlgParserParser.NOTEQUAL, "Test Object Inequality",
          AlgParserParser.EQUAL, "Test Object Equality",
          AlgParserParser.DWIM_MATCHES, "DWIM Pattern Match",
          AlgParserParser.FIELDSOP, "Filter Fields from a Map",
          AlgParserParser.SEARCHOP, "Search",
          AlgParserParser.ASOP, "Introduce pipe variable name",
          //AlgParserParser.GASSIGN, "Global Variable Assign",
          AlgParserParser.LASSIGN, "Local Variable Assign");

  private final Map<Integer, String> operatorTokenTypes =
      map(
          AlgParserParser.ADDOP, "+", // arithmetic
          AlgParserParser.DIVOP, "/",
          AlgParserParser.MULOP, "*",
          AlgParserParser.SUBOP, "-",
          AlgParserParser.REMOP, "%",
          AlgParserParser.ANDOP, "AND", // logical
          AlgParserParser.OROP, "OR",
          AlgParserParser.NOTOP, "NOT",
          AlgParserParser.EQUAL, "==", // equality
          AlgParserParser.NUMEQUAL, "=",
          AlgParserParser.NOTEQUAL, "!=",
          AlgParserParser.INOP, "IN",
          AlgParserParser.NUMGE, ">=", // comparison
          AlgParserParser.NUMGT, ">",
          AlgParserParser.NUMLE, "<=",
          AlgParserParser.NUMLT, "<",
          AlgParserParser.DWIM_MATCHES, "=~",
          AlgParserParser.SEARCHOP, "SEARCH",
          AlgParserParser.FIELDSOP, "FIELDS",
          AlgParserParser.ASOP, "AS",
          //          AlgParserParser.GASSIGN, "::=", // assignment
          AlgParserParser.LASSIGN, ":=");
}
