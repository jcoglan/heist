module Heist
  class Runtime
    class Macro
      
      # +Expansion+ is responsible for expanding syntactic forms matched by
      # successful +Macro+ calls. Any successful +Macro+ call returns an
      # +Expansion+ object, which is used as a signal to the evaluator that the
      # +Expression+ contained in the expansion should be inlined into the
      # syntax tree.
      class Expansion
        attr_reader :expression
        
        # An +Expansion+ is initialized using the lexical +Scope+ of the +Macro+
        # and the +Scope+ of the macro call site (both required for hygiene
        # purposes), plus a +Cons+ representing the expansion template and a
        # +Matches+ object containing the input expressions to be transcribed
        # using the template. After initialization the expanded +Expression+ is
        # available via the +Expansion+ object's +expression+ attribute.
        def initialize(lexical_scope, calling_scope, template, matches)
          @lexical_scope = lexical_scope
          @calling_scope = calling_scope
          @hygienic      = lexical_scope.runtime.hygienic?
          @expression    = expand(template, matches)
        end
        
        # Accepts a template object (a +Cons+, +Identifier+ or similar) and a
        # +Matches+ instance and returns the result of expanding the +template+
        # using the +matches+. The +depth+ and +ignoring_ellipses+ arguments are
        # for internal state maintainance as we recursively expand the template
        # forms. +depth+ indicates the current repetition depth, i.e. how many
        # ellipses follow the current subtemplate, and +ignoring_ellipses+ is
        # +true+ iff we're expanding a template in which ellipses should be
        # transcribed verbatim. This is an R6RS feature; if a template opens
        # with an ellipsis, we transcribe the rest of the template as normal
        # except that any ellipses in the template are inserted as ellipses into
        # the output, without causing their preceeding forms to repeat.
        #
        # From the R5RS spec
        # http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html
        #
        # When a macro use is transcribed according to the template of the
        # matching <syntax rule>, pattern variables that occur in the template
        # are replaced by the subforms they match in the input. Pattern
        # variables that occur in subpatterns followed by one or more instances
        # of the identifier '...' are allowed only in subtemplates that are
        # followed by as many instances of '...'. They are replaced in the
        # output by all of the subforms they match in the input, distributed as
        # indicated. It is an error if the output cannot be built up as
        # specified.
        #
        # Identifiers that appear in the template but are not pattern variables
        # or the identifier '...' are inserted into the output as literal
        # identifiers. If a literal identifier is inserted as a free identifier
        # then it refers to the binding of that identifier within whose scope
        # the instance of 'syntax-rules' appears. If a literal identifier is
        # inserted as a bound identifier then it is in effect renamed to prevent
        # inadvertent captures of free identifiers.
        #
        def expand(template, matches, depth = 0, ignoring_ellipses = false)
          case template
          
            when Cons then
              # Return NULL if the template is an empty list
              return Cons::NULL if template.null?
              
              # If the template is a list opening with an ellipsis, expand the
              # rest of the list, transcribing ellipses verbatim
              return expand(template.cdr.car,
                            matches, depth, true) if template.car == ELLIPSIS
              
              result, last, repeater, template_pair = nil, nil, nil, template
              
              # Set up a closure to push forms onto the output. Needs to track
              # both the head (+result+, for returning) and the tail (+last+,
              # for appending new forms). Links each inserted form to its
              # containing +Cons+ to enable further expansions to be inlined.
              push = lambda do |value|
                pair = Cons.new(value)
                pair.hosts(value)
                result ||= pair
                last.cdr = pair if last
                last = pair
              end
              
              # Iterate over the template, inserting matches as we go
              while Cons === template_pair and not template_pair.null?
                cell = template_pair.car
                
                # Increment the repetition depth if the current subtemplate is
                # followed by an ellipsis and we are not treating ellipses as
                # literals
                followed_by_ellipsis = ( Cons === template_pair.cdr &&
                                         template_pair.cdr.car == ELLIPSIS) &&
                                       !ignoring_ellipses
                
                dx = followed_by_ellipsis ? 1 : 0
                
                repeater = cell if followed_by_ellipsis
                
                # Once we reach an ellipsis, expand the preceeding form the
                # correct number of times depending on the +matches+
                if cell == ELLIPSIS and not ignoring_ellipses
                  matches.expand!(repeater, depth + 1) do
                    push[expand(repeater, matches, depth + 1)]
                  end
                
                # If the current subtemplate is not an ellipsis and is not
                # followed by an ellipsis, expand it and push the result onto
                # the output
                else
                  push[expand(cell, matches, depth + dx,
                              ignoring_ellipses)] unless followed_by_ellipsis
                end
                
                template_pair = template_pair.cdr
              end
              
              # Handle the tail of improper list templates
              last.cdr = expand(template_pair, matches, depth, ignoring_ellipses) unless last.nil?
              result
          
            # TODO this is very similar to how we handle lists -> refactor
            when Vector then
              result, repeater = Vector.new, nil
              push = lambda { |value| result << value }
              
              template.each_with_index do |cell, template_index|
                
                followed_by_ellipsis = (template[template_index+1] == ELLIPSIS) && !ignoring_ellipses
                dx = followed_by_ellipsis ? 1 : 0
                
                repeater = cell if followed_by_ellipsis
                
                if cell == ELLIPSIS and not ignoring_ellipses
                  matches.expand!(repeater, depth + 1) do
                    push[expand(repeater, matches, depth + 1)]
                  end
                else
                  push[expand(cell, matches, depth + dx,
                              ignoring_ellipses)] unless followed_by_ellipsis
                end
              end
              result
          
            when Identifier then
              # If the template is a pattern variable, return the current match
              # for that variable. See +Matches+ to see how repeated patterns
              # are handled.
              return matches.get(template) if matches.has?(template)
              
              # Otherwise, if using unhygienic macros, return the template
              # verbatim as a new symbol.
              return Identifier.new(template) unless @hygienic
              
              # If using hygienic macros: bind the identifier to the macro's
              # lexical scope if it is defined there, otherwise rename it as
              # appropriate to avoid clashes with variables in the calling scope.
              @lexical_scope.defined?(template) ?
                  Binding.new(template, @lexical_scope, false) :
                  rename(template)
          
            else
              template
          end
        end
        
        # Returns a new +Identifier+ that does clash with any of the names
        # visible in the <tt>Expansion</tt>'s calling scope.
        def rename(id)
          return id unless @calling_scope.defined?(id)
          i = 1
          i += 1 while @calling_scope.defined?("#{id}#{i}")
          Identifier.new("#{id}#{i}", id)
        end
      end
      
    end
  end
end

