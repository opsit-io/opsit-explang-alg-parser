package io.opsit.explang.autosuggest;

import java.util.Map;

public class Suggestion {
  public String text;
  public String kind;
  public String suffix;
  public Map<String,String> properties;

  /**
   * Construct suggestion for code completion.
   */
  public Suggestion(String text,
                    String kind,
                    String suffix,
                    Map<String,String> properties) {
    this.text = text;
    this.kind = kind;
    this.suffix = suffix;
    this.properties = properties;
  }

  /**
   * Construct suggestion for code completion.
   */
  public Suggestion(String text,
                    String kind,
                    Map<String,String> properties) {
    this.text = text;
    this.kind = kind;
    this.suffix = "";
    this.properties = properties;
  }

  @Override
  public String toString() {
    final StringBuilder buf = new StringBuilder();
    buf.append("(");
    buf.append("text=").append(text).append(",");
    buf.append("kind=").append(kind).append(",");
    buf.append("suffix=").append(suffix).append(",");
    buf.append("properties=").append(properties).append(",");
    return buf.toString();
  }
}

