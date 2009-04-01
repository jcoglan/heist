module Heist
  # +RubyParser+ parses non-string code given as Ruby data such as arrays. It
  # allows Lisp-style code to be expressed inline with Ruby, for example:
  #
  #   scheme = Heist::Runtime.new
  #   scheme.exec [:define, [:square, :x], [:*, :x, :x]]
  #   scheme.exec [:square, 9]
  #   #=> 81
  #
  # The above API uses +RubyParser+ behind the scenes to turn Ruby data into
  # Heist runtime objects such as +Cons+ based lists before execution.
  #
  class RubyParser
    
    # Parses a single piece of Ruby data in
    def parse(source)
      case source
        when Array then
          Runtime::Cons.construct(source) { |cell| parse(cell) }
        when Symbol then
          Runtime::Identifier.new(source)
        else
          source
      end
    end
    
  end
end

