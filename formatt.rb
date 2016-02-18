#!/usr/bin/env ruby

Signal.trap "INT" do
  exit
end

B = /<b>([^<]*)<\/b>/
CODE = /<span[^>]*>([^<]*)<\/span>/
EM = /<em>([^<]*)<\/em>/
H1 = /<h1>([^<]*)<\/h1>/
H2 = /<h2>([^<]*)<\/h2>/
H3 = /<h3>([^<]*)<\/h3>/
H4 = /<h4>([^<]*)<\/h4>/
HINTBOX = /<div class="hintbox">/
ITALIC = /<i>([^<]*)<\/i>/
MISC = /\r|<p>|<\/p>|<\/div>/
PRE = /<pre[^>]*>([^<]*)<\/pre>/

html_string = ARGF.read

html_string.gsub!(MISC, '')
html_string.gsub!(/(<[^>]*)\n/, '\1')
html_string.gsub!(HINTBOX, 'HINTBOX \1')
html_string.gsub!(B, '\1')
html_string.gsub!(PRE, '```\1```')
html_string.gsub!(ITALIC, '_\1_')
html_string.gsub!(EM, '**\1**')
html_string.gsub!(H1, '# \1')
html_string.gsub!(H2, '## \1')
html_string.gsub!(H3, '### \1')
html_string.gsub!(H4, '#### \1')
html_string.gsub!(H1, '# \1')
html_string.gsub!(CODE, '`\1`')

puts html_string
