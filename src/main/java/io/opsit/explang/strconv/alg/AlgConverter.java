package io.opsit.explang.strconv.alg;

import io.opsit.explang.IStringConverter;

public class AlgConverter implements IStringConverter {
  @Override
  public String convert(String str) {
    return null == str
      ? null
      : ("-".equals(str)
         ? str
         : str.toUpperCase().replace('-', '_'));
  }
}
