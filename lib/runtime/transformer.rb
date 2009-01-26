module Heist
  class Runtime
    
    class Transformer < Function
      ELLIPSIS = '...'
      
      # TODO:   * figure out the right rule to pick
      #         * execute body in the same scope
      #         * throw an error if no rules match
      def call(scope, cells)
        rule, closure = *rule_for(cells, scope)
        return nil unless rule
        rule[1..-1].map { |part| Heist.value_of(part, closure) }.last
      end
      
    private
      
      def rule_for(cells, scope)
        match = nil
        @body.each do |rule|
          next if match
          bindings = rule_bindings(rule, cells, scope)
          match = [rule, bindings] if bindings
        end
        match
      end
      
      def rule_bindings(rule, cells, scope)
        pattern  = rule.first
        bindings = Scope.new(@scope)
        tokens   = pattern[1..-1]
        success  = true
        
        return false if cells.size > tokens.size &&
                        tokens.last.to_s != ELLIPSIS
        
        tokens.each_with_index do |cell, i|
          next unless success
          
          if tokens[i+1].to_s == ELLIPSIS
            bindings[cell] = Splice.new(cells[i..-1])
            break
          else
            success = false unless cells[i]
            bindings[cell] = Binding.new(cells[i], scope, false)
          end
        end
        success ? bindings : false
      end
    end
    
    class Splice
      attr_reader :cells
      def initialize(cells)
        @cells = cells
      end
    end
    
  end
end

