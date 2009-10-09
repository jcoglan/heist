$VERBOSE = nil
$dir = File.expand_path(File.dirname(__FILE__))

$args = ARGV.map { |opt| "-#{opt}" }

require $dir + "/../lib/heist"
require $dir + "/../lib/bin_spec"
require "test/unit"

Class.new(Test::Unit::TestCase) do
  @env = nil
  
  def setup
    return @env if @env
    @env = Heist::Runtime.new(Heist::BIN_SPEC.parse($args))
    
    puts @env.info unless defined?(@@info_printed)
    @@info_printed = true
    
    @env.define('assert') do |value|
      assert(value)
    end
    @env.define('assert-equal') do |expected, actual|
      assert_equal(expected, actual)
    end
    @env.syntax('assert-raise') do |scope, cells|
      exception = Heist.const_get(cells.car.to_s)
      assert_raise(exception) { scope.eval(cells.cdr.car) }
    end
  end
  
  %w[   booleans
        numbers
        lists
        vectors
        strings
        equivalence
        arithmetic
        define_values
        define_functions
        closures
        functional
        let
        conditionals
        file_loading
        macros
        delay
        protection
        
  ].each do |test|
    define_method('test_' + test) do
      @env.run($dir + '/scheme_tests/' + test)
    end
  end
  
  def test_cons
    cons = Heist::Runtime::Cons
    
    empty = cons[[]]
    assert empty.list?
    assert !empty.pair?
    assert_equal cons::NULL, empty
    assert_equal 0, empty.length
    
    single = cons[[4]]
    assert single.list?
    assert single.pair?
    assert_equal 1, single.length
    
    multi = cons[[2,4,7]]
    assert multi.list?
    assert multi.pair?
    assert_equal 3, multi.length
    
    multi.tail.cdr = 8
    assert multi.pair?
    assert !multi.list?
    assert_raise(Heist::TypeError) { multi.size }
    
    nested = cons[[2,4,6,cons.new(7,8)]]
    assert nested.list?
    assert nested.pair?
    assert_equal 4, nested.length
    assert !nested.cdr.cdr.cdr.car.list?
    assert nested.cdr.cdr.cdr.car.pair?
    assert_equal 8, nested.cdr.cdr.cdr.car.cdr
  end
  
  def test_macro_hygiene
    @env.run($dir + '/scheme_tests/' + (@env.hygienic? ? 'hygienic' : 'unhygienic'))
  end
  
  def test_continuations
    return if @env.stackless?
    @env.run($dir + '/scheme_tests/continuations')
  end
  
  def test_quotation
    cons = Heist::Runtime::Cons
    c = cons.method(:new)
    
    assert_equal 7,                     @env.eval("(+ 3 4)")
    assert_equal [:+, 3, 4],            @env.eval("'(+ 3 4)").to_ruby
    assert cons === @env.eval("'(+ 3 4)")
    assert_equal 7,                     @env.eval("(+ '3 4)")
    assert_equal c[1,8],                @env.eval("'(1 . 8)")
    assert_equal c[1,8],                @env.eval("`(1 . 8)")
    assert_equal [:+, [:-, 7, 9], 4],   @env.eval("'(+ (- 7 9) 4)").to_ruby
    assert_equal [7, 9, 6],             @env.eval("`(7 ,(+ 4 5) 6)").to_ruby
    assert cons === @env.eval("`(7 ,(+ 4 5) 6)")
    assert_equal [3, 7, 6, 2, 6, 9],    @env.eval("`(3 7 6 ,@((lambda () '(2 6))) 9)").to_ruby
    assert_equal [1, 2, 10],            @env.eval("`(1 2 ,(+ 4 6))").to_ruby
    assert_equal [3, 2, 9, 8],          @env.eval("`(,(/ 9 3) 2 ,@(list 9 8))").to_ruby
    assert_equal [1, 2, 4, 9, 8, 5],    @env.eval("`(,@(list 1 2) 4 ,@(list 9 8) 5)").to_ruby
    assert_equal c[9,c[8,c[5,7]]],      @env.eval("`(,@(list 9 8) 5 . 7)")
    assert_equal c[9,c[8,24]],          @env.eval("`(,@(list 9 8) . ,(* 4 6))")
    assert_equal [:quote, []],          @env.eval("''()").to_ruby
  end
  
  def test_birdhouse
    return unless @env.lazy?
    @env.eval('(load "birdhouse")')
    
    @env.eval <<-CODE
      (define factorial (Y
        (lambda (rec)
          (lambda (x)
            (if (= x 0) 1 (* x (rec (- x 1))))))))
    CODE
    assert_equal (1..6).inject { |a,b| a*b },
                 @env.eval("(factorial 6)")
    
    assert_equal 45, @env.eval("((K 45) 6)")
  end
  
  def test_ruby_execution
    expr = [[:lambda, [:x], [:+, 1, :x]], 3]
    assert_equal 4, @env.exec(expr)
    list = Heist.parse(expr)
    assert Heist::Runtime::Cons === list
    assert_equal expr, list.to_ruby
    
    cons = Heist::Runtime::Cons.method(:new)
    assert_equal cons[3,cons[4,5]], Heist.parse([3, 4, :'.', 5])
    assert_equal [3, 4, :'.', 5], cons[3,cons[4,5]].to_ruby
  end
end

