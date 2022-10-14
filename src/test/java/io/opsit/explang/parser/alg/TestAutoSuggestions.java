package io.opsit.explang.parser.alg;

import static io.opsit.explang.Utils.map;
import static io.opsit.explang.Utils.set;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import io.opsit.explang.Compiler;
import io.opsit.explang.Keyword;
import io.opsit.explang.Symbol;
import io.opsit.explang.autosuggest.SourceInfo;

@RunWith(Parameterized.class)
public class TestAutoSuggestions extends AbstractTest {
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
  String in;
  int inPos;
  Set<String> expOut;
  String expExc;
        
  public TestAutoSuggestions(String in, int inPos, Set<String> expOut, String expExc) {
    this.in = in;
    this.inPos = inPos;
    this.expOut = expOut;
    this.expExc = expExc;
    //this.expExc
  }
        
  @Parameters
  public static Collection<Object[]> data() {
    return Arrays.asList(new Object[][] {
        {"AB", 2, set("ABACUS","ABBA") , null},
        {"ABA", 2, set("ABACUS") , null},
        {"ABA", 3, set("ABACUS") , null},
        {"ABA ", 3, set("==", "===", "=~", "<=", ":=", "OR", "IN", "%",
                        "(", "*", "+", "-", ".", "/", "AND","[",
                        "!=", "<", "=", ">", ">=", "FIELDS","AS","|","SEARCH","->") , null},
        {"ABACUS ", 6, set("==", "===", "=~", "<=", ":=", "OR", "IN", "%",
                           "(", "*", "+", "-", ".","/", "AND", "[",
                           "!=", "<", "=", ">", ">=","FIELDS","AS","|","SEARCH","->") , null},// FIXME: -> ABACUS
        {"ABACUS ", 7, set() , null}, // FIXME -> 17 selections
        {"ABACUS  ", 7, set("==", "===", "=~", "<=", ":=", "OR", "IN", "%",
                            "(", "*", "+", "-", ".", "/", "AND", "[",
                            "!=", "<", "=", ">", ">=", "FIELDS","AS","|","SEARCH","->") , null}
      });
  }

  // @Test
  // public void testExprParseCtx() {
  //     log("\n\n TEST #: "+(testNum++));
  //    Compiler compiler = new Compiler();
  //     AlgReader parser = new AlgReader();
  //    compiler.setParser(parser);
  //     try {
        
  //         log("\n\nIN:  "+in);
  //        //ASTNList resultList
        
  //         //Object output = stripAST(resultList);
  //         //log("OUT: " + output);
  //         //log("ExP: " + expOut);
  //        for (int i = 1; i < in.length(); i++) {
  //        String str = in.substring(0, i);
  //        log("SUGG["+str+","+(i-1)+"]: " +
  //                   parser.autoSuggest(in.substring(0,i),
  //                              compiler.newCtx(),
  //                              i-1,
  //                              true, true, true, true));
  //        }
  //     } catch (Exception ex) {
  //         if (null != expExc && ex.getMessage().contains(expExc)) {
  //             System.err.println("Got expected exception: " + ex);
  //         } else {
  //             Assert.fail("expected exception containing: " + expExc + " but got: " + ex);
  //         }
  //     }
  // }

  protected static String spacer(int l) {
    StringBuilder b = new StringBuilder(l);
    for (int i = 0; i < l; i++) {
      b.append(' ');
    }
    return b.toString();
  }

  @Test
  public void testExprParseCtx() {
    log("\n\n TEST #: "+(testNum++));
    Compiler compiler = new Compiler();
    AlgParser parser = new AlgParser();
    Compiler.ICtx ctx = compiler.newCtx(map("ABBA", 1,
                                            "ABACUS", 2));
    compiler.setParser(parser);
    try {
        
      log("\n\nSTRING: "+in);
      log(String.format("P=%04d: "+spacer(inPos)+"^\n", inPos));

        
      SourceInfo si = parser.autoSuggest(in,
                                         ctx,
                                         inPos,
                                         true, true, true, true);
      log("SourceInfo: "+si);
      log("  Expected: "+expOut);

      Assert.assertNotNull(si);
      Assert.assertNotNull(si.suggestions);
      //Assert.assertEquals(expOut.size(), si.suggestions.size());
      Set<String> variants = si.suggestions.stream().map(s -> s.text).collect(Collectors.toSet());
      Assert.assertEquals(expOut,variants);
        
      //ASTNList resultList
      //Object output = stripAST(resultList);
      //log("OUT: " + output);
      //log("ExP: " + expOut);
      //for (int i = 1; i < in.length(); i++) {
      //String str = in.substring(0, i);
      //log("["+str+","+(i-1)+"]: " +
    } catch (Exception ex) {
      if (null != expExc && ex.getMessage().contains(expExc)) {
        log("Got expected exception: " + ex);
      } else {
        flushLog();
        Assert.fail("expected exception containing: " + expExc + " but got: " + ex);
      }
    }
  }

    
  /* no more used
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
        
     }*/

}

