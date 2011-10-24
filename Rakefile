require 'rubygems'
require File.expand_path('../lib/heist', __FILE__)

file "lib/builtin/compiled_library.rb" do |t|
  library_source = %w[util logic numeric list character string vector].
                   map { |l| File.read "lib/builtin/lib/#{l}.scm" }.
                   join("\n\n")
  
  program = Heist.parse(library_source).convert!
  File.open(t.name, 'w') { |f| f.write 'program ' + program.to_ruby.inspect }
end

task :compile => "lib/builtin/compiled_library.rb"

namespace :spec do
  task :r5rs do
    procedures = Dir['r5rs/*.html'].
                 map { |f| File.read(f) }.
                 join("\n").
                 split(/\n+/).
                 grep(/(syntax|procedure)\:/).
                 map { |s| s.gsub(/<\/?[^>]+>/, '').
                             scan(/\(([^\) ]+)/).
                             flatten.
                             first }.
                 uniq.
                 compact.
                 map { |s| s.gsub('&lt;', '<').
                             gsub('&gt;', '>') }
    
    scope = Heist::Runtime.new.top_level
    procedures.each do |proc|
      message = scope.defined?(proc) ? scope.exec(proc) : 'MISSING'
      puts "    %-32s %-48s" % [proc, message]
    end
  end
end

