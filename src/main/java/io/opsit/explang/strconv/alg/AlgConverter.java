package io.opsit.explang.strconv.alg;

import io.opsit.explang.IStringConverter;

public class AlgConverter implements IStringConverter {
  public String convert(String str) {
    return null == str ? null : str.toUpperCase().replaceAll("-", "_");
  }
}
