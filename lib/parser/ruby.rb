module Heist
  class RubyParser
    
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

