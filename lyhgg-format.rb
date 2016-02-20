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
IMG_POSITION = /"(left|center|right)"/
IMG_SIZE = / width="\d*" height="\d*"/
MISC = /\r|<p>|<p [^>]*>|<\/p>|<\/div>/

html_string = ARGF.read

html_string.gsub!(MISC, '')
html_string.gsub!(/(<[^>]*)\n/, '\1')
html_string.gsub!(HINTBOX, 'HINTBOX \1')
html_string.gsub!(B, '\1')
html_string.gsub!(ITALIC, '_\1_')
html_string.gsub!(IMG_POSITION, '"img-\1"')
html_string.gsub!(IMG_SIZE, '')
html_string.gsub!(EM, '**\1**')
html_string.gsub!(H1, '# \1')
html_string.gsub!(H2, '## \1')
html_string.gsub!(H3, '### \1')
html_string.gsub!(H4, '#### \1')
html_string.gsub!(H1, '# \1')
html_string.gsub!(CODE, '`\1`')
html_string.gsub!(/([^!]! |[^\?]\? |[^\.]\. )/, "\\1\n")
html_string.gsub!(/ *$/, '')
html_string.gsub!(/&gt;/, '>')
html_string.gsub!(/&amp;/, '&')

html_string.gsub!(/`/, '\\\`')

PRE = /<pre[^>]*>|<\/pre>/
html_string.gsub!(PRE, "```\n")

indent = false
html_string.each_line do |line|
  if indent
    puts "    #{line}"
  else
    puts line.gsub(/([^,], )/, "\\1\n")
  end

  if line.match(/```/)
    indent = indent ? false : true
  end
end
