module Heist
  class Runtime
    
    class Identifier
      include Expression
      
      extend Forwardable
      def_delegators(:@metadata, :[], :[]=)
      def_delegators(:@name, :to_s)
      alias :inspect :to_s
      
      def initialize(name)
        @name = name.to_s
        @metadata = {}
      end
      
      def ==(other)
        Identifier === other and @name == other.to_s
      end
    end
    
  end
end

