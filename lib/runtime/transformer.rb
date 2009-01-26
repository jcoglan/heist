module Heist
  class Runtime
    
    class Transformer < Function
      ELLIPSIS = '...'
      
      def initialize(*args)
        super
        @renames = {}
      end
      
      # TODO:   * figure out the right rule to pick
      #         * execute body in the same scope
      #         * throw an error if no rules match
      def call(scope, cells)
        rule, bindings = *rule_for(cells, scope)
        return nil unless rule
        expanded = expand_template(rule[1..-1], bindings)
        expanded.map { |part| Heist.value_of(part, scope) }.last
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
        bindings = Scope.new
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
            bindings[cell] = cells[i]
          end
        end
        success ? bindings : false
      end
      
      def expand_template(template, bindings)
        template.map do |cell|
          case cell
          
          when List then
            expand_template(cell, bindings)
          
          when Identifier then
            binding_scope = [bindings, @scope].find { |env| env.defined?(cell) }
            binding_scope ?
                binding_scope[cell] :
                rename(cell)
          else
            cell
          end
        end
      end
      
      def rename(identifier)
        @renames[identifier.to_s] ||= Identifier.new("__#{identifier}__")
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

