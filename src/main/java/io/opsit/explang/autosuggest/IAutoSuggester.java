package io.opsit.explang.autosuggest;

import java.util.List;

import io.opsit.explang.Compiler;

public interface IAutoSuggester {
  public SourceInfo   autoSuggest(String input,
                                  Compiler.ICtx context,
                                  int curPos,
                                  boolean returnTokenization,
                                  boolean returnErrors,
                                  boolean returnSuggestions,
                                  boolean filterSuggestions);
  public Tokenization tokenize(String input, int curPos);

  public List<String> getErrors(String input, int curPos);
    
}
