package io.opsit.explang.autosuggest;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class SourceInfo {
  /**
   * Construct SourceInfo with source data.
   */
  public SourceInfo(String rawString, int rawPos) {
    super();
    this.rawString = rawString;
    this.rawPos = rawPos;
    this.errors = new ArrayList<String>();
    this.suggestions = new ArrayList<Suggestion>();
    this.tokenization = new Tokenization();
  }

  public Tokenization tokenization;
  // unparsed line
  public String rawString;
  public int rawPos;

  public List<String> errors;
  public List<Suggestion> suggestions;

  public void addAll(Collection<Suggestion> items) {
    this.suggestions.addAll(items);
  }

  @Override
  public String toString() {
    StringBuilder buf = new StringBuilder(255);
    buf.append("Suggestions(");
    buf.append("rawLine='").append(rawString).append("', ");
    buf.append("rawPos=").append(rawPos).append(", ");
    buf.append("tokenization=").append(tokenization).append(",");
    buf.append("errors=").append(errors).append(",");
    buf.append("selections").append(suggestions).append("");
    buf.append(")");
    return buf.toString();
  }
}
