# Quotations taken from the R5RS spec
# http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html
module Heist
  class Runtime
    
    class Macro < MetaFunction
      ELLIPSIS = '...'
      
      def initialize(*args)
        super
        @renames = {}
      end
      
      # TODO:   * throw an error if no rules match
      def call(scope, cells)
        rule, bindings = *rule_for(cells, scope)
        return nil unless rule
        @splices = {}
        expanded = expand_template(rule.last, bindings)
        Expansion.new(expanded)
      end
      
    private
      
      def rule_for(cells, scope)
        @body.each do |rule|
          bindings = rule_bindings(rule.first[1..-1], cells)
          return [rule, bindings] if bindings
        end
        nil
      end
      
      # More formally, an input form F matches a pattern P if and only if:
      # 
      #     * P is a non-literal identifier; or
      #     * P is a literal identifier and F is an identifier with the
      #       same binding; or
      #     * P is a list (P1 ... Pn) and F is a list of n forms that match
      #       P1 through Pn, respectively; or
      #     * P is an improper list (P1 P2 ... Pn . Pn+1) and F is a list
      #       or improper list of n or more forms that match P1 through Pn,
      #       respectively, and whose nth 'cdr' matches Pn+1; or
      #     * P is of the form (P1 ... Pn Pn+1 <ellipsis>) where <ellipsis>
      #       is the identifier '...' and F is a proper list of at least n forms,
      #       the first n of which match P1 through Pn, respectively, and
      #       each remaining element of F matches Pn+1; or
      #     * P is a vector of the form #(P1 ... Pn) and F is a vector of n
      #       forms that match P1 through Pn; or
      #     * P is of the form #(P1 ... Pn Pn+1 <ellipsis>) where <ellipsis>
      #       is the identifier '...' and F is a vector of n or more forms the
      #       first n of which match P1 through Pn, respectively, and each
      #       remaining element of F matches Pn+1; or
      #     * P is a datum and F is equal to P in the sense of the 'equal?'
      #       procedure.
      # 
      # It is an error to use a macro keyword, within the scope of its
      # binding, in an expression that does not match any of the patterns.
      # 
      def rule_bindings(tokens, input, bindings = Scope.new, splicing = false)
        idx = 0
        tokens.each_with_index do |token, i|
          
          is_ellipsis = (token.to_s == ELLIPSIS)
          followed_by_ellipsis = (tokens[i+1].to_s == ELLIPSIS)
          
          case token
            # TODO handle improper lists and vectors
            #      when they are implemented
            
            when List then
              value = nil 
              while List === input[idx] &&
                    value = rule_bindings(token, input[idx], bindings,
                                          followed_by_ellipsis || splicing) &&
                    followed_by_ellipsis
                idx += 1
              end
              return nil if value.nil?
              idx += 1 if not followed_by_ellipsis
            
            when Identifier then
              return nil if @formals.include?(token.to_s) &&
                            token.to_s != input[idx].to_s
              
              if splicing
                bindings[token] = Splice.new unless bindings.defined?(token)
                bindings[token] << input[idx]
                idx += 1
              elsif followed_by_ellipsis
                splice = input[idx..-1]
                bindings[token] = Splice.new(input[idx..-1])
                idx = input.size
              else
                next if is_ellipsis
                return nil unless input[idx]
                bindings[token] = input[idx]
                idx += 1
              end
            
            else
              return nil unless token == input[idx]
              idx += 1
          end
        end
        idx == input.size ? bindings : nil
      end
      
      # When a macro use is transcribed according to the template of the
      # matching <syntax rule>, pattern variables that occur in the template
      # are replaced by the subforms they match in the input. Pattern variables
      # that occur in subpatterns followed by one or more instances of the
      # identifier '...' are allowed only in subtemplates that are followed
      # by as many instances of '...'. They are replaced in the output by all
      # of the subforms they match in the input, distributed as indicated. It
      # is an error if the output cannot be built up as specified.
      # 
      # Identifiers that appear in the template but are not pattern variables
      # or the identifier '...' are inserted into the output as literal
      # identifiers. If a literal identifier is inserted as a free identifier
      # then it refers to the binding of that identifier within whose scope
      # the instance of 'syntax-rules' appears. If a literal identifier is
      # inserted as a bound identifier then it is in effect renamed to prevent
      # inadvertent captures of free identifiers.
      # 
      def expand_template(template, bindings)
        case template
        
          when List then
            result = List.new
            template.each_with_index do |cell, i|
              if cell.to_s == ELLIPSIS
                # TODO throw error if we have mismatched sets of splices
                n = @splices.map { |k,v| v.size }.uniq.first - 1
                n.times { result << expand_template(template[i-1], bindings) }
                @splices = {}
              else
                value = expand_template(cell, bindings)
                result << value unless Splice === value
              end
            end
            result
          
          when Identifier then
            value   = bindings[template] if bindings.defined?(template)
            value ||= Binding.new(template, @scope) if @scope.defined?(template)
            value ||= rename(template)
            @splices[template.to_s] = value if Splice === value
            (Splice === value && !(value.empty?)) ? value.shift : value
          
          else
            template
        end
      end
      
      def rename(id)
        @renames[id.to_s] ||= Identifier.new("::#{id}::")
      end
      
      class Expansion
        attr_reader :expression
        def initialize(expression)
          @expression = expression
        end
      end
      
      class Splice < Array
        def initialize(*args)
          super(*args)
          @index = 0
        end
        
        def shift
          value = self[@index]
          @index += 1
          @index = 0 if @index >= size
          value
        end
      end
    end
    
  end
end

