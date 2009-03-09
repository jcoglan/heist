# Quotations taken from the R5RS spec
# http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html
module Heist
  class Runtime
    
    class Macro < Syntax
      ELLIPSIS = Identifier.new('...')
      RESERVED = %w[_ ...]
      
      class << self
        def pattern_vars(pattern, excluded = [])
          case pattern
            when Identifier then
              name = pattern.to_s
              return nil if excluded.include?(name) or
                            RESERVED.include?(name)
              [name]
            when Cons then
              pattern.map { |cell| pattern_vars(cell, excluded) }.
                      flatten.
                      compact
          end
        end
      end
      
      %w[expansion splice matches].each do |klass|
        require RUNTIME_PATH + 'callable/macro/' + klass
      end
      
      def call(scope, cells)
        rule, matches = *rule_for(cells)
        
        return Expansion.new(@scope, scope, rule.cdr.car, matches) if rule
        
        raise SyntaxError.new(
          "Bad syntax: no macro expansion found for #{Cons.new(@name, cells)}")
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
        return nil
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
      def rule_matches(pattern, input, matches = nil, depth = 0)
        matches ||= Matches.new(Macro.pattern_vars(pattern, @formals))
        case pattern
        
          when Cons then
            return nil unless Cons === input
            pattern_pair, input_pair = pattern, input
            
            skip = lambda { pattern_pair = pattern_pair.cdr }
            
            while not pattern_pair.null?
              token = pattern_pair.car
              followed_by_ellipsis = (pattern_pair.cdr.car == ELLIPSIS)
              dx = followed_by_ellipsis ? 1 : 0
              
              matches.descend!(Macro.pattern_vars(token, @formals),
                               depth + dx) if followed_by_ellipsis
              
              skip[] and next if token == ELLIPSIS
              
              consume = lambda { rule_matches(token, input_pair.car, matches, depth + dx) }
              
              unless followed_by_ellipsis
                return nil unless consume[]
                input_pair = input_pair.cdr
                skip[] and next
              end
              
              input_pair = input_pair.cdr while not input_pair.null? and
                                                followed_by_ellipsis and
                                                consume[]
              skip[]
            end
            return nil unless input_pair.null?
        
          when Identifier then
            return (pattern == input) if @formals.include?(pattern.to_s)
            matches.put(pattern, input)
            return nil if input.nil?
        
          else
            return pattern == input ? true : nil
        end
        matches
      end
      
    end
    
  end
end

