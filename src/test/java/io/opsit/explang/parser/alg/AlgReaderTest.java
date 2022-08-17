package io.opsit.explang.parser.alg;

import java.lang.SuppressWarnings;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.regex.Pattern;
import io.opsit.explang.ASTN;
import io.opsit.explang.ASTNList;
import io.opsit.explang.Keyword;
import io.opsit.explang.ParseCtx;
import io.opsit.explang.Symbol;
import io.opsit.version.Version;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

@RunWith(Parameterized.class)
public class AlgReaderTest extends AbstractTest {
  static int testNum = 0;
    
  public static List<Object> list(Object ... objs) {
    return Arrays.asList(objs);
  }

  public static Symbol sym(String str) {
    return new Symbol(str);
  }
  public static Keyword keyword(String str) {
    return new Keyword(str);
  }

  Object expOut;
  String in;
  String expExc;
  @SuppressWarnings("rawtypes")
  Comparator cmp;
        
  public AlgReaderTest(String in, Object expOut, String expExc) {
    this.in = in;
    this.expOut = expOut;
    this.expExc = expExc;
  }
        
  @Parameters
  public static Collection<Object[]> data() throws Exception {
    final String fancyliteral = "foo\nbar\r\nbooo\tbla";
    final String fancyescliteral = "\"foo\\nbar\\r\\nbooo\\tbla\"";
    final Object[][] tests = new Object[][] {
      { "1$", Integer.valueOf(1), "test:1:1: Parser: token recognition error at: '$'"},
      //{ "()", list(), null},
      //{ "foo", sym("foo"), null},

      { "1", Integer.valueOf(1), null},
      { "111", Integer.valueOf(111), null},
      { "+1", list(new Symbol("+"), 1), null},
      // FIXME: why? why + is parsed as part of literal and - not?
      { "-1", list(new Symbol("-"), 1), null},
      { "1.1", Double.valueOf(1.1), null},
      { "1.0", Double.valueOf(1.0), null},
      { "1.0f", Float.valueOf((float)1.0), null},
      { "1.0F", Float.valueOf((float)1.0), null},
      { "1.0d", Double.valueOf(1.0), null},
      { "1.0D", Double.valueOf(1.0), null},
      { "-1.0D", list( new Symbol("-"),Double.valueOf(1.0)), null},
      { "1L", Long.valueOf(1), null},
      { "1i", Integer.valueOf(1), null},
      { "1I", Integer.valueOf(1), null},
      { "1s", Short.valueOf((short)1), null},
      { "1S", Short.valueOf((short)1), null},
      { "1b", Byte.valueOf((byte)1), null},
      { "1B", Byte.valueOf((byte)1), null},
      { "null", null, null},
      { "Null", null, null},
      { "Nil",  null, null},
      { "NULL", null, null},
      { "NIL",  null, null},
      { "TRUE", Boolean.TRUE, null},
      { "true", Boolean.TRUE, null},
      { "True", Boolean.TRUE,  null},                                 
      { "False", Boolean.FALSE, null},
      { "false", Boolean.FALSE, null},
      { "FALSE", Boolean.FALSE,  null},                               
      { "\"foo\"", "foo", null},
      { "\""+fancyliteral+"\"", fancyliteral, null},
      { fancyescliteral, fancyliteral, null},
      { "1 + 2", list(sym("+"),1,2) , null},
      { "1 + 2 + 3", list(sym("+"),list(sym("+"),1,2), 3) , null},
      { "1 + (2 + 3)", list(sym("+"),1,list(sym("+"),2,3)) , null},      
      { "1 * 2", list(sym("*"),1,2) , null},
      { "1 * 2 * 3", list(sym("*"),list(sym("*"),1,2), 3) , null},
      { "1 * (2 * 3)", list(sym("*"),1,list(sym("*"),2,3)) , null},
      { "1 * (2)", list(sym("*"),1,2) , null},
      { "1 + (2)", list(sym("+"),1,2) , null},
      { "print(1)", list(sym("print"),1) , null},
      { "print(1,2,3)", list(sym("print"),1,2,3) , null},
      { "+(1,2,3)", list(sym("+"),1,2,3) , null},
      { "2 * +(1,2,3)", list(sym("*"),2,list(sym("+"),1,2,3)) , null},
      {"2 + +(1,2,3)", list(sym("+"), 2, list(sym("+"), 1, 2, 3)), null},

      { "1 and 2", list(sym("AND"),1,2) , null},
      { "1 AND 2", list(sym("AND"),1,2) , null},         
      { "and(1,2)", list(sym("and"),1,2) , null},
      { "AND(1,2)", list(sym("AND"),1,2) , null},
      { "and(1)", list(sym("and"),1) , null},
      { "AND(1)", list(sym("AND"),1) , null},
      { "and(1,2,3)", list(sym("and"),1,2,3) , null},
      { "AND(1,2,3)", list(sym("AND"),1,2,3) , null},
      { "and(1,2)", list(sym("and"),1,2) , null},
      { "and(1)", list(sym("and"), 1), null},

      { "1 or 2", list(sym("OR"),1,2) , null},
      { "1 OR 2", list(sym("OR"),1,2) , null},       
      { "or(1,2)", list(sym("or"),1,2) , null},
      { "OR(1,2)", list(sym("OR"),1,2) , null},
      { "or(1)", list(sym("or"),1) , null},
      { "OR(1)", list(sym("OR"),1) , null},
      { "or(1,2,3)", list(sym("or"),1,2,3) , null},
      { "OR(1,2,3)", list(sym("OR"),1,2,3) , null},
      { "or(1,2)", list(sym("or"),1,2) , null},
      { "or(1)", list(sym("or"), 1), null},

      { "not(1)", list(sym("NOT"), 1), null},
      { "NOT(1)", list(sym("NOT"), 1), null},
      { "not 1", list(sym("NOT"), 1), null},
      { "NOT 1", list(sym("NOT"), 1), null},

      { "1 === 2", list(sym("==="), 1, 2), null},
         
      { "a", sym("a"), null},
      { "_ABYRVALG", sym("_ABYRVALG"), null},
      { ":a", list(sym("QUOTE"), sym("a")), null},
      // FIXME: make way to make keywords
      //{ ":a", keyword(":a"), null},
      { ":_ABYRVALG", list(sym("QUOTE"),sym("_ABYRVALG")), null},
      //{ ":_ABYRVALG", keyword(":_ABYRVALG"), null},
                  
      { "quote(AND)", list(sym("quote"),sym("AND")), null},
      { "quote(FOO)", list(sym("quote"),sym("FOO")), null},
      { "3 IN LIST(1,2,3)", list(sym("IN"), 3, list(sym("LIST"), 1 ,2 ,3)), null},
      { "IN(3,LIST(1,2,3))", list(sym("IN"), 3, list(sym("LIST"), 1 ,2 ,3)), null},

      { "# 1 + 2 comment\n 44\n  # 22\n", 44,null},
      { "#= 1 + 2 comment\n 22\n  =# 44\n", 44,null},

      { "1 +", 1, "test:1:2: Parser: extraneous input '+' expecting <EOF>"},
      { "r\"123\"", Pattern.compile("123"), null},
      { "r\"123\"i", Pattern.compile("123", Pattern.CASE_INSENSITIVE), null},
      { "r\"\\\"123\\b\"x", Pattern.compile("\"123\\b", Pattern.COMMENTS), null},
      { "g\"gl*b\"", Pattern.compile("gl.*b"), null},
                
      { "f\"+\"", list(sym("FUNCTION"),sym("+")), null},
                
      { "v\"1.2.3-4+z\"", Version.mkSemVersion(1L,2L,3L,list(4),list("z")), null},

      {"i\"foo\"", "foo", null},
      {"i\"foo=$(100+1)\"", list(sym("STR"),"foo=",list(sym("+"), 100, 1)), null},
      {"i\"foo=$(100+1), bar=$(200+1)\"", list(sym("STR"),"foo=",list(sym("+"), 100, 1),", bar=",list(sym("+"), 200, 1)), null},
      {"i\"foo=$(100+1), bar=$(200+1)!!!\"", list(sym("STR"),"foo=",list(sym("+"), 100, 1),", bar=",list(sym("+"), 200, 1),"!!!"), null},
      {"i\"$(100+1)$(200+1)\"", list(sym("STR"),list(sym("+"), 100, 1),list(sym("+"), 200, 1)), null},
      {"i\"foo=$(STR(\"(\",\"x\",\"=\",\"y\",\")\n\"))!!!\"", list(sym("STR"),"foo=",list(sym("STR"),"(","x","=","y",")\n"), "!!!"), null},
      {"i\"fo\\$o\"", "fo$o", null},
      {"i\"fo\\$\\no\"", "fo$\no", null},
      {"i\"fo\\$(1+2)o\"", "fo$(1+2)o", null},
      {"i\"fo$(1+2)=\\$(1+2)o\"", list(sym("STR"),"fo", list(sym("+"),1,2),"=$(1+2)o"), null},
                
                
      { "Function(x) 1; end", list(sym("LAMBDA"),list(sym("x")), 1), null},
      { "function(x) 1; End", list(sym("LAMBDA"),list(sym("x")), 1), null},
      { "begin 1; end", list(sym("PROGN"), 1), null},
      { "BEGIN 1; END", list(sym("PROGN"), 1), null},
      { "HASHMAP(\"name\",\"foo\",\"surname\",\"bar\") fields name,surname",
        list(sym("DWIM_FIELDS"), list(sym("HASHMAP"),
                                      "name", "foo", "surname", "bar"), list(sym("LIST"),"name", "surname")), null},
      { "1 AS foo | foo", list(sym("AS->"), 1, sym("foo"), sym("foo")), null},

      { "1 AS foo | foo | bar()", list(sym("AS->"),1,sym("foo"),sym("foo"),list(sym("bar"))), null},
      { "1 | foo | bar()", list(sym("@->"),1,sym("foo"),list(sym("bar"))), null},
      { "1 | * 2 ", list(sym("*"),1,2), null},
      { "LET([[a,1],[b,2],[c,3]], STR(\"a=\",a,\" b=\",b,\" c=\",c))",
        list(sym("LET"),
             list(list(sym("a"), 1), list(sym("b"), 2), list(sym("c"), 3)),
             list(sym("STR"), "a=", sym("a"), " b=", sym("b"), " c=", sym("c"))),null}
    };
    return filterTests(tests);
  }

  @Test
  public void testExprParse() {
    try {
      log("\n\n TEST #: "+(testNum++));
            
      AlgParser parser = new AlgParser();
      //try {
      log("\n\nIN:  "+in);
      ParseCtx pctx = new ParseCtx("test");
      ASTNList resultList = parser.parse(pctx, in, 1);
      Object output = stripAST(resultList);
      log("OUT: " + output);
      log("ExP: " + expOut);
      log("STR: " + parser.sexpToString(output));
      if (null != resultList.problem) {
        log("ERR: " + resultList.problem+ " caused by " + resultList.problem.getCause());
        //resultList.problem.printStackTrace();
        log("PROBLEM STACK TRACE: " + resultList.problem.getStackTrace());
      }

      Assert.assertTrue(output instanceof List);
      @SuppressWarnings("unchecked")
        List <Object>outList = (List<Object>) output;
      Assert.assertEquals(1, outList.size());
      Object outExpr = outList.get(0);
      assertEqual(expOut, outExpr);
      if (null != expExc) {
        Assert.assertNotNull(resultList.problem);
        Assert.assertTrue(resultList.problem.getMessage().contains(expExc));
      } else {
        Assert.assertNull(resultList.problem);
      }
    } catch (Throwable t) {
      flushLog();
      throw t;
    }
  }

  private void assertEqual(Object expected, Object actual) {
    if (null == expected) {
      Assert.assertEquals(expected, actual);
    } else if (expected instanceof Pattern) {
      Assert.assertTrue(actual instanceof Pattern);
      Assert.assertNotNull(actual);
      Assert.assertEquals(expected.toString(), actual.toString());
      Assert.assertEquals(((Pattern)expected).flags(), ((Pattern)actual).flags());
    } else {
      Assert.assertEquals(expected, actual);
    }
  }
  private Object stripAST(ASTN obj) {
    if (obj.isList()) {
      List <Object> newLst = new ArrayList<Object>();
      for (ASTN el : ((ASTNList)obj)) {
        newLst.add(stripAST(el));
      }
      return newLst;
    } else  {
      return obj.getObject();
    } 
    //  throw new RuntimeException("Unexpected onject in AST" + obj);
        
  }

}

