module Heist
  class Runtime
    
    # The +Character+ class is used to represent Scheme's notion of characters,
    # objects that represent single ASCII glyphs. They are written in Scheme code
    # as the symbols <tt>#\\</tt> followed by the desired glyph.
    #
    # Characters can be mapped back and forth to integer character codes and are
    # thus sortable. This class uses Ruby's +Comparable+ mixin to sort characters
    # by their ASCII code.
    #
    # In addition to single glyphs, there are a few named characters that
    # represent whitespace characters: see +SPECIAL+ for named characters
    # supported by Heist.
    #
    class Character
      include Comparable
      attr_reader :glyph
      
      # List of special character names and the strings they represent
      SPECIAL = {
        "space"   => " ",
        "newline" => "\n",
        "tab"     => "\t"
      }
      
      # A +Character+ is initialized using either a single-character string,
      # or a string containing the name of a special whitespace character. If
      # a name is used, it must appear as a key in +SPECIAL+ otherwise an
      # exception is thrown.
      def initialize(glyph)
        raise SyntaxError.new("There is no character named '#{glyph}'") if glyph.size > 1 and SPECIAL[glyph].nil?
        @glyph = glyph
      end
      
      # Returns the ASCII code for the +Character+ as an integer.
      def char_code
        code, char = nil, to_s
        char.each_byte { |x| code = x }
        code
      end
      alias :to_i :char_code
      
      # Returns +true+ iff the two characters represent the same glyph.
      def ==(other)
        Character === other and
        ( @glyph               == other.glyph or
          SPECIAL[@glyph]      == other.glyph or
          SPECIAL[other.glyph] == @glyph )
      end
      
      # Returns the difference between the receiver's charcode and the argument's
      # charcode, used for the +Comparable+ operations.
      def <=>(other)
        char_code - other.char_code
      end
      
      # Returns a new +Character+ representing the uppercase version
      # of the receiver.
      def upcase
        self.class.new(to_s.upcase)
      end
      
      # Returns a new +Character+ representing the lowercase version
      # of the receiver.
      def downcase
        self.class.new(to_s.downcase)
      end
      
      # Returns a +String+ of unit length containing only the +Character+.
      def to_s
        SPECIAL[@glyph] || @glyph
      end
      
      # Returns a Scheme-style string representation of the +Character+.
      def inspect
        "#\\#{ @glyph }"
      end
    end
    
  end
end

