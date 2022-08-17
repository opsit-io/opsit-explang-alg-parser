package io.opsit.explang.parser.alg;

import java.util.List;
import java.util.ArrayList;

public abstract class AbstractTest {
  protected boolean isVerbose = false;
  protected StringBuilder logbuf = new StringBuilder();

  protected static Object[] ONLY = {"__ONLY__"};
    
  protected void log(String str) {
    if (isVerbose) {
      System.out.println(str);
    } else {
      logbuf.append(str).append('\n');
    }

  }

  protected void clearLog() {
    this.logbuf.setLength(0);
  }

  protected void flushLog() {
    if (logbuf.length() == 0) {
      System.out.println(logbuf.toString());
      clearLog();
    }
  }

  protected static List<Object[]> filterTests(Object[][] tests) {
    List<Object[]> result = new ArrayList<Object[]>();
    for (int i = 0; i < tests.length; i++) {
      Object []test = tests[i];
      if (test[0].equals(ONLY[0])) {
        result.clear();
        result.add(tests[i+1]);
        break;
      } else {
        result.add(test);
      }
    }
    return result;
  }
}


