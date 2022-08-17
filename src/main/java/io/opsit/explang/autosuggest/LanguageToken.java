package io.opsit.explang.autosuggest;

public class LanguageToken {
  public final String text;
  public final String type;
  public final int startPos;

  public static final String TOKKIND_SYMBOL = "symbol";
  public static final String TOKKIND_OPERATOR = "operator";
  public static final String TOKKIND_NUMBER = "number";
  public static final String TOKKIND_NIL = "nil";
  public static final String TOKKIND_BOOLEAN = "boolean";
  public static final String TOKKIND_STRING = "string";
  public static final String TOKKIND_KEYWORD = "keyword";
  // public static final String TOKKIND_BRACES= "braces";
  // public static final String TOKKIND_BRACKETS= "brackets";
  public static final String TOKKIND_PARENTHESES = "parentheses";
  public static final String TOKKIND_COMMENTS = "comments";
  public static final String TOKKIND_WHITESPACE = "whitespace";
  public static final String TOKKIND_KEYWORDS = "keywords";

  /**
   * Construct token.
   */
  public LanguageToken(String text, String type, int startPos) {
    this.text = text;
    this.type = type;
    this.startPos = startPos;
  }

  /**
   * Return string representation of parsed token.
   */
  public String toString() {
    return "LanguageToken("
        + "text='"
        + text
        + "', type='"
        + type
        + "', startPos="
        + startPos
        + ")";
  }
}
