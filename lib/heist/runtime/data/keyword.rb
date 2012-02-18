module Heist
  class Runtime
    
    # A +Keyword+ is a a self-quoting identifier ending in ":". The
    # separate class is merely a convenience for implementing the
    # +keyword?+ predicate.
    class Keyword < Identifier
      
      # Since keywords need never be renamed they don't need an
      # +original_name+ argument.
      def initialize(name)
        super(name)
      end
    end
    
  end
end

