module Heist
  class Runtime
    
    class Character
      SPECIAL = {
        "space"   => " ",
        "newline" => "\n",
        "tab"     => "\t"
      }
      
      def initialize(glyph)
        raise SyntaxError.new("There is no character named '#{glyph}'") if glyph.size > 1 and SPECIAL[glyph].nil?
        @glyph = glyph
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

