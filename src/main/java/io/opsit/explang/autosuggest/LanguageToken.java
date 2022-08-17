package io.opsit.explang.autosuggest;

public class LanguageToken {
  public final String text;
  public final String type;
  public final int startPos;

  public final static String TOKKIND_SYMBOL = "symbol";
  public final static String TOKKIND_OPERATOR = "operator";
  public final static String TOKKIND_NUMBER = "number";
  public final static String TOKKIND_NIL = "nil";
  public final static String TOKKIND_BOOLEAN = "boolean";
  public final static String TOKKIND_STRING = "string";
  public final static String TOKKIND_KEYWORD = "keyword";
  //public final static String TOKKIND_BRACES= "braces";
  //public final static String TOKKIND_BRACKETS= "brackets";
  public final static String TOKKIND_PARENTHESES= "parentheses";
  public final static String TOKKIND_COMMENTS= "comments";
  public final static String TOKKIND_WHITESPACE= "whitespace";
  public final static String TOKKIND_KEYWORDS = "keywords";

  public LanguageToken(String text, String type, int startPos) {
    this.text = text;
    this.type = type;
    this.startPos=startPos;
  }

  public String toString() {
    return "LanguageToken("+
      "text='" + text +
      "', type='" + type +
      "', startPos=" + startPos + ")";
  }
}
