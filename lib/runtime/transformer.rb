module Heist
  class Runtime
    
    class Transformer < Function
      # TODO:   * figure out the right rule to pick
      #         * execute body in the same scope
      def call(scope, cells)
        closure = Scope.new(scope)
        rule = @body.first
        cells.each_with_index do |cell, i|
          closure[rule.first[i+1]] = Binding.new(cell, scope, false)
        end
        rule[1..-1].each { |part| Heist.value_of(part, closure) }
      end
    end
    
  end
end

