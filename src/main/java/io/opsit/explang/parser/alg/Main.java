package io.opsit.explang.parser.alg;


public class Main extends io.opsit.explang.Main {
  // FIXME: ugly customization with inheritance,
  //        need configuration mechanism?
  /**
   * Entry for REPL with default Algebraic syntax.
   */
  public static void main(String []argv) throws Exception {
    Main main = new Main();
    main.getParsers().add(0, "alg");
    main.getFuncConverters().add(0, "alg");
    main.runWithArgs(argv);
  }
}


