module Heist
  class Runtime
    
    class Character
      include Comparable
      attr_reader :glyph
      
      SPECIAL = {
        "space"   => " ",
        "newline" => "\n",
        "tab"     => "\t"
      }
      
      def initialize(glyph)
        raise SyntaxError.new("There is no character named '#{glyph}'") if glyph.size > 1 and SPECIAL[glyph].nil?
        @glyph = glyph
      end
      
      def char_code
        code, char = nil, to_s
        char.each_byte { |x| code = x }
        code
      end
      
      def ==(other)
        Character === other and
        ( @glyph               == other.glyph or
          SPECIAL[@glyph]      == other.glyph or
          SPECIAL[other.glyph] == @glyph )
      end
      
      def <=>(other)
        char_code - other.char_code
      end
      
      def upcase
        self.class.new(to_s.upcase)
      end
      
      def downcase
        self.class.new(to_s.downcase)
      end
      
      def to_s
        SPECIAL[@glyph] || @glyph
      end
      
      def inspect
        "#\\#{ @glyph }"
      end
    end
    
  end
end

