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
      if (null != syntaxErrors && syntaxErrors.size() > 0) {
        problem =
            new ParserExceptions(
                mkPctx(pctx, syntaxErrors.get(0)),
                syntaxErrors.stream()
                    .map(se -> se2ParserException(pctx, se))
                    .collect(Collectors.toList()));
      } // else {
      ExprVisitor visitor = new ExprVisitor();
      result = (ASTNList) visitor.visit(pst.tree);
      if (null == result) {
        new ASTNList(list(), pctx.clone());
      }
      if (null != problem) {
        result.problem = problem;
      }
    } catch (Exception ex) {
      globalProblem = new ParserException(pctx.clone(), "AlgParser Exception", ex);
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
      lexer.addErrorListener(listener);
      // create a buffer of tokens pulled from the lexer​
      // FIXME: customize type of token stream
      CommonTokenStream tokenStream = new CommonTokenStream(lexer);
      // TokenStream tokens = new UnbufferedTokenStream(lexer);
      // create a parser that feeds off the tokens buffer​
      AlgParserParser parser = new AlgParserParser(tokenStream);
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
      // ParseCtx pctx = makePctx(ctx);
      ASTN e = (ASTN) visit(ctx.expr());
      // ASTN at = new ASTNLeaf(symbol("@"), pctx);
      // ASTNList result = new ASTNList(list(e, at), pctx);
      return e;
    }

    /*
    @Override
    public Object visitTh_at_infix_expr(AlgParserParser.Th_at_infix_exprContext ctx) {
    final ParseCtx pctx = makePctx(ctx);
    final ASTN startExpr = (ASTN) visit(ctx.getChild(0));
    final ASTN targetExpr = (ASTN) visit(ctx.getChild(2));

    ASTNList result =
    new ASTNList(list(new ASTNLeaf(symbol("->"), pctx),
    startExpr,
    targetExpr), pctx);

    //for (int i = 4; i < ctx.getChildCount(); i+=2) {
    //    ASTN expr = (ASTN) visit(ctx.getChild(i));
    //    result.add(expr);
    //}
    return result;
    }*/

    // @Override
    // public Object visitSeq_iterator_expr(AlgParserParser.Seq_iterator_exprContext ctx) {
    //     final ParseCtx pctx = makePctx(ctx);
    //     final ASTN seqExpr = (ASTN) visit(ctx.getChild(0));
    //     final ASTN iterExpr = (ASTN) visit(ctx.getChild(3));
    //     ASTN result =
    //         // FIXME: lambda arg name
    //         //        what if inside AS->?
    //         ASTNize(list(symbol("MAP"),
    //                      list(symbol("LAMBDA"),
    //                           list(symbol("%1%")),
    //                           list(symbol("@->"),
    //                                symbol("%1%"),
    //                                iterExpr)),
    //                      seqExpr), pctx);

    //     return result;
    // }

    /*private ASTN ASTNize(Object param, ParseCtx ctx) {
    if (param instanceof List) {
      // ;ASTN lst = new ASTN
      List<ASTN> astnList = new ArrayList<ASTN>(((List) param).size());
      for (Object obj : (List) param) {
        final ASTN node = ASTNize(obj, ctx);
        astnList.add(node);
      }
      return new ASTNList(astnList, ctx);
    } else if (param instanceof ASTN) {
      return (ASTN) param;
    } else {
      return new ASTNLeaf(param, ctx);
    }
    }*/
    // expr (as var)? (| expr )+
    // it works somewhat non-trivially:
    // the operator is right-associative, so originally
    // it gets parsed as
    // (A | (B | ...X))
    // and then recursively flattened into
    // (-> A B .. X)
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
      
      boolean hasRest = null != ctx.rest ;
      boolean hasKWRest = null != ctx.okeys ;
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
          trArgList.add(transArg(kwargs.get(kwargs.size()-1)));
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
      final AlgParserParser.ExprContext vars = ctx.expr(0);
      final AlgParserParser.ExprContext vals = ctx.expr(1);
      final AlgParserParser.ExprContext ret = ctx.expr(2);
      final ASTN varsASTN = (ASTN) visit(vars);
      final ASTN valsASTN = (ASTN) visit(vals);
      final ASTN retASTN = null == ret ? null : (ASTN) visit(ret);
      final ASTNList block = (ASTNList)visit(ctx.block());

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
          new ASTNList(list(new ASTNLeaf(symbol("DWIM_SEARCH"), pctx), obj, pat), pctx);
      return result;
    }

    // expr  DWIM_MATCHES expr                         # dwim_matches_expr
    @Override
    public Object visitFields_expr(AlgParserParser.Fields_exprContext ctx) {
      final ParseCtx pctx = makePctx(ctx);
      final int childCount = ctx.getChildCount();
      final ASTN obj = (ASTN) visit(ctx.getChild(0));
      final ASTNList ks = new ASTNList(list(new ASTNLeaf(symbol("LIST"), pctx)), pctx);
      for (int i = 2; i < childCount; i += 2) {
        String field = ctx.getChild(i).getText();
        ks.add(new ASTNLeaf(field, pctx));
      }
      ASTNList result =
          new ASTNList(list(new ASTNLeaf(symbol("DWIM_FIELDS"), pctx), obj, ks), pctx);

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
      //final ASTN var = new ASTNLeaf(symbol(ctx.getChild(0).getText()), pctx);
      final ASTN var = (ASTN) visit(ctx.expr(0));
      final ASTN val = (ASTN) visit(ctx.expr(1));
      //final ASTN val = (ASTN) visit(ctx.getChild(2));
      //final String symName = ctx.op.getType() == AlgParserParser.GASSIGN ? "SETQ" : "SETV";
      final ASTNList result = new ASTNList(list(new ASTNLeaf(symbol("SETF"), pctx), var, val), pctx);
      return result;
    }

    // SYMBOL GASSIGN expr
    @Override
    public Object visitGassign_expr(AlgParserParser.Gassign_exprContext ctx) {
      ParseCtx pctx = makePctx(ctx);
      final ASTN var = new ASTNLeaf(symbol(ctx.getChild(0).getText()), pctx);
      final ASTN val = (ASTN) visit(ctx.getChild(2));
      final ASTNList result = new ASTNList(list(new ASTNLeaf(symbol("SETQ"), pctx), var, val), pctx);
      return result;
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
          opsym = symbol("=");
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
      ParseCtx pctx = makePctx(ctx);
      final ASTN opASTN = new ASTNLeaf(symbol("IN"), pctx);
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
        final ASTNList args = (ASTNList) visit(ctx.getChild(2));
        for (int i = 0; i < args.size(); i++) {
          ASTN arg = args.get(i);
          if (arg.isList()) {
            final ASTNList argLst = (ASTNList)arg;
            if (argLst.size() == 3
                && !argLst.get(0).isList()
                && symbol("SETF").equals(argLst.get(0).getObject())
                && !argLst.get(1).isList()
                && (argLst.get(1).getObject() instanceof Symbol)) {
              result.add(new ASTNLeaf(new Keyword(":" + argLst.get(1).getObject()), argLst.get(1).getPctx()));
              result.add(argLst.get(2));
              continue;
            }
          }
          result.add(arg);
        }

      }
      return result;
    }
  }

  /** Make new Parse context on basis of ANTLR parsing information. */
  public ParseCtx makePctx(ParserRuleContext rctx) {
    // FIXME: really map th information
    final ParseCtx pctx =
        new ParseCtx(
            rctx.getText(), /* line*/ -1, /* pos */ -1, /* offset */ 0, rctx.getText().length());
    return pctx;
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

  @Override
  public String formatArgSpec(List<String> spec) {
    StringBuilder buf = new StringBuilder(64);
    if (null != spec) {
      boolean isPrefix = false;
      for (int idx = 0; idx < spec.size(); idx++) {
        String str = spec.get(idx);
        if (idx > 0) {
          if (!isPrefix) {
            buf.append(",");
          }
          buf.append(" ");
        }
        buf.append(str);
        isPrefix = str.startsWith("&");
      }
    } else {
      buf.append(ArgSpec.ARG_REST).append("args");
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
          AlgParserParser.GASSIGN, "Global Variable Assign",
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
          AlgParserParser.GASSIGN, "::=", // assignment
          AlgParserParser.LASSIGN, ":=");
}
