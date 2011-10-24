module Heist
  class Runtime
    
    # An +Identifier+ is used to represent any name that appears in Scheme
    # code. We might throw it away and use symbols at some point. I had this
    # idea for storing metadata on +Identifier+ objects to speed things up
    # but it pretty much came to nothing. Its one saving grace is that it
    # needs to be treated as an +Expression+ class, and also that we need
    # to store some extra metadata on +Identifier+ objects, and I don't want
    # to modify core Ruby classes
    class Identifier
      include Expression
      
      extend Forwardable
      def_delegators(:@name, :to_s)
      alias :inspect :to_s
      
      # An +Identifier+ is initialized using a string that becomes the
      # name of the identifier. The optional second parameter is used to
      # specify the original name of an identifier if it has been renamed
      # during a macro expansion.
      def initialize(name, orginal_name = nil)
        @name = name.to_s
        @orginal_name = (orginal_name || name).to_s
      end
      
      def name
        @orginal_name
      end
      
      # Returns +true+ if the receiver has the same name as the argument.
      def ==(other)
        return true if Binding === other and other == self
        Identifier === other and @orginal_name.downcase == other.name.downcase
      end
      
      # Returns a raw Ruby representation of the identifier, for which
      # we use symbols.
      def to_ruby
        @name.to_s.to_sym
      end
    end
    
  end
end

