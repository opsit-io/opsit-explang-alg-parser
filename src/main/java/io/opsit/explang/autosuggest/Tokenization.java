package io.opsit.explang.autosuggest;

import java.util.List;
import java.util.ArrayList;


public class Tokenization {
  public String token;
  // cursor pos in current token
  public int tokenPos;
  public List<LanguageToken> tokens;
  // index in list of tokens
  public Integer tokenIndex;

  public String toString() {
    StringBuilder buf = new StringBuilder(32);
    buf.append("Tokenization(");
    buf.append("token='").append(token).append("', ");
    buf.append("tokenPos=").append(tokenPos).append(", ");
    buf.append("tokenIndex=").append(tokenIndex).append(", ");
    buf.append("tokens=").append(tokens);
    buf.append(")");
    return buf.toString();
  }

  public Tokenization() {
    this.tokens = new ArrayList<LanguageToken>();
    this.token = "";
    this.tokenPos = 0;
  }
    
  public Tokenization(List<LanguageToken> tokens, Integer tokenIndex, String token, int tokenPos) {
    this.tokens = tokens;
    this.tokenIndex = tokenIndex;
    this.token = token;
    this.tokenPos = tokenPos;
  }
}
