module Heist
  class Runtime
    
    # A +Keyword+ is a a self-quoting identifier ending in ":". The
    # separate class is merely a convenience for implementing the
    # +keyword?+ predicate.
    class Keyword < Identifier
      
      attr_reader :keyword_name
      
      # Since keywords need never be renamed they don't need an
      # +original_name+ argument.
      def initialize(name)
        @keyword_name = name
        super("#{name}:")
      end
      
    end
    
  end
end

