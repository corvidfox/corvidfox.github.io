-- Lua filter to add filename using code cell language
-- Written by mcanouil (https://github.com/quarto-dev/quarto-cli/discussions/10400)
function CodeBlock(code)
  local lang = code.attr.classes[1] 
  if lang == "cell-code" then 
    _, _, matchedLang = string.find(code.text, "^`+%{%{([^%}]*)%}%}")
    lang = matchedLang or lang
  elseif lang ~= nil and lang:find('{{', 1, true) == 1 then
    _, _, matchedLang = string.find(lang, "{{+(.-)}}+")
    if matchedLang then
      lang = matchedLang
    end
  end
  return quarto.DecoratedCodeBlock({
    filename = lang,
    code_block = code:clone()
  })
end