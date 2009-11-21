dir = File.dirname(__FILE__)

require dir + '/../lib/heist'
require dir + '/../lib/bin_spec'

INIT_FLAGS = [''] + %w[-c -l -u -uc -ul]
TEST_FILES = %w[equivalence]

INIT_FLAGS.each do |flags|
  options = Heist::BIN_SPEC.parse([flags])
  runtime = Heist::Runtime.new(options)
  
  puts "\n" + runtime.info
  runtime.run(dir + '/support.scm')
  
  TEST_FILES.each { |file| runtime.run "#{dir}/#{file}.scm" }
  runtime.exec [:'test-summary']
end

