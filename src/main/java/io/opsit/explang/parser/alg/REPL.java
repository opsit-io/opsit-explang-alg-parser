package io.opsit.explang.parser.alg;


public class REPL extends io.opsit.explang.REPL {
  // FIXME: ugly customization with inheritance,
  //        need configuration mechanism?
  public static void main(String []argv) throws Exception {
    REPL repl = new REPL();
    repl.getParsers().add(0, "alg");
    repl.getFuncConverters().add(0, "alg");
    repl.runWithArgs(argv);
  }
}


