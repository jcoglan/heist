# Quotations taken from the R5RS spec
# http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html
module Heist
  class Runtime
    
    class Macro < Syntax
      ELLIPSIS = '...'
      
      %w[expansion splice matches].each do |klass|
        require RUNTIME_PATH + 'callable/macro/' + klass
      end
      
      def initialize(scope, *args)
        super
        @hygienic = scope.runtime.hygienic?
      end
      
      def call(scope, cells)
        @calling_scope = scope
        rule, matches = *rule_for(cells)
        
        return Expansion.new(expand_template(rule.cdr.car, matches)) if rule
        
        input = cells.map { |c| c.to_s } * ' '
        raise SyntaxError.new(
          "Bad syntax: no macro expansion found for (#{@name} #{input})")
      end
      
      def to_s
        "#<macro:#{ @name }>"
      end
      
    private
      
      def rule_for(cells)
        @body.each do |rule|
          matches = rule_matches(rule.car.cdr, cells)
          return [rule, matches] if matches
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
      def rule_matches(pattern, input, matches = Matches.new, depth = 0)
        case pattern
        
          when Cons then
            return nil unless Cons === input
            pattern_pair, input_pair = pattern, input
            
            skip = lambda { pattern_pair = pattern_pair.cdr }
            
            while not pattern_pair.null?
              token = pattern_pair.car
              followed_by_ellipsis = (pattern_pair.cdr.car.to_s == ELLIPSIS)
              dx = followed_by_ellipsis ? 1 : 0
              
              matches.depth = depth + dx
              skip[] and next if token.to_s == ELLIPSIS
              
              consume = lambda { rule_matches(token, input_pair.car, matches, depth + dx) }
              return nil unless value = consume[] or followed_by_ellipsis
              skip[] and next unless value
              input_pair = input_pair.cdr
              
              input_pair = input_pair.cdr while not input_pair.null? and
                                                followed_by_ellipsis and
                                                consume[]
              skip[]
            end
            return nil unless input_pair.null?
        
          when Identifier then
            return (pattern.to_s == input.to_s) if @formals.include?(pattern.to_s)
            matches.put(pattern, input)
            return nil if input.nil?
        
          else
            return pattern == input ? true : nil
        end
        matches
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
      def expand_template(template, matches, depth = 0, inspection = false)
        case template
        
          when Cons then
            result, last, repeater, template_pair = nil, nil, nil, template
            
            push = lambda do |value|
              return if value == Cons::NULL
              pair = Cons.new(value)
              result ||= pair
              last.cdr = pair if last
              last = pair
            end
            
            while not template_pair.null?
              cell = template_pair.car
              followed_by_ellipsis = (template_pair.cdr.car.to_s == ELLIPSIS)
              dx = followed_by_ellipsis ? 1 : 0
              
              matches.inspecting(depth + 1) if followed_by_ellipsis and
                                               not inspection
              
              repeater = cell if followed_by_ellipsis
              
              if cell.to_s == ELLIPSIS and not inspection
                matches.expand! { push[expand_template(repeater, matches, depth + 1)] }
                matches.depth = depth
              else
                inspect = inspection || (followed_by_ellipsis && depth + 1)
                value = expand_template(cell, matches, depth + dx, inspect)
                push[value] unless inspect
              end
              
              template_pair = template_pair.cdr
            end
            result
        
          when Identifier then
            return matches.get(template) if matches.defined?(template)
            return Identifier.new(template) unless @hygienic
            
            @scope.defined?(template) ?
                Binding.new(template, @scope, false) :
                rename(template)
        
          else
            template
        end
      end
      
      def rename(id)
        return id unless @calling_scope.defined?(id)
        i = 1
        i += 1 while @calling_scope.defined?("#{id}#{i}")
        Identifier.new("#{id}#{i}")
      end
    end
    
  end
end

