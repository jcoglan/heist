module Heist
  class Runtime
    
    # +Stack+ is responsible for executing code by successively evaluating
    # expressions. It provides fine-grained intermediate result inspection
    # to support the Scheme notion of continuations, working with the +Frame+
    # and +Body+ classes to evaluate expressions and function bodies piece
    # by piece. Using the +Stack+ engine allows the creation of +Continuation+
    # functions, which save the current state of the stack (i.e. the state
    # of any unfinished expressions and function bodies) and allow it to be
    # resumed at some later time.
    #
    # +Stack+ inherits from +Array+, and is a last-in-first-out structure:
    # the next expression evaluated is always the last expression on the
    # stack.
    #
    # You should think of the +Stack+ as an array of +Frame+ objects that
    # hold expressions and track their progress. For example, take the
    # expression:
    #
    #   (+ (- (* 8 9) (/ 21 7)) 4)
    #
    # Evaluating it involves evaluating each subexpression to fill in holes
    # where we expect values; when all the holes in an expression have been
    # filled, we can apply the resulting function to the arguments and get
    # a value. Evaluating this expression causes the stack to evolve as
    # follows, where STATE lists the expressions on the stack and <tt>[]</tt>
    # represents a hole that is waiting for a value:
    #
    #   PUSH:  (+ (- (* 8 9) (/ 21 7)) 4)
    #   STATE: ([] [] 4)
    #   
    #   PUSH:  +
    #   VALUE: #<procedure:+>
    #   STATE: (#<procedure:+> [] 4)
    #   
    #   PUSH:  (- (* 8 9) (/ 21 7))
    #   STATE: (#<procedure:+> [] 4), ([] [] [])
    #   
    #   PUSH:  -
    #   VALUE: #<procedure:->
    #   STATE: (#<procedure:+> [] 4), (#<procedure:-> [] [])
    #   
    #   PUSH:  (* 8 9)
    #   STATE: (#<procedure:+> [] 4), (#<procedure:-> [] []), ([] 8 9)
    #   
    #   PUSH:  *
    #   VALUE: #<procedure:*>
    #   STATE: (#<procedure:+> [] 4), (#<procedure:-> [] []), (#<procedure:*> 8 9)
    #   
    #   VALUE: 72
    #   STATE: (#<procedure:+> [] 4), (#<procedure:-> 72 [])
    #   
    #   PUSH:  (/ 21 7)
    #   STATE: (#<procedure:+> [] 4), (#<procedure:-> 72 []), ([] 21 7)
    #   
    #   PUSH:  /
    #   VALUE: #<procedure:/>
    #   STATE: (#<procedure:+> [] 4), (#<procedure:-> 72 []), (#<procedure:/> 21 7)
    #   
    #   VALUE: 3
    #   STATE: (#<procedure:+> [] 4), (#<procedure:-> 72 3)
    #   
    #   VALUE: 69
    #   STATE: (#<procedure:+> 69 4)
    #   
    #   VALUE: 73
    #
    # So we find that <tt>(+ (- (* 8 9) (/ 21 7)) 4)</tt> gives the value 73.
    # Whenever a value is returned by a subexpression we must inspect it to
    # see if a +Continuation+ has been called. All this inspection of
    # intermediate values takes time; if you don't need full +Continuation+
    # support, use the faster +Stackless+ engine instead.
    #
    class Stack < Array
      attr_reader :value
      
      # Pushes a new +Frame+ or +Body+ onto the +Stack+ and then executes
      # the resulting code until the pushed frame returns a value, which
      # is then returned.
      def <<(frame)
        super
        clear!(size - 1)
      end
      
      # Creates and returns a copy of the stack, which represents the current
      # computational state: any unfinished expressions and function bodies
      # are stored in the stack. Pass +false+ to discard the final frame,
      # which will typically be a call to <tt>(call/cc)</tt> when creating
      # a +Continuation+.
      def copy(keep_last = true)
        copy = self.class.new
        range = keep_last ? 0..-1 : 0...-1
        self[range].each do |frame|
          copy[copy.size] = frame.clone
        end
        copy
      end
      
      # Fills a hole in the final +Frame+ on the +Stack+ by replacing the
      # given epxression +subexpr+ with the given +value+. If the +value+
      # is a +Frame+, this frame is pushed onto the stack rather than filling
      # a hole in the previous frame.
      def fill!(subexpr, value)
        return self[size] = value if Frame === value
        return @value = value if empty?
        last.fill!(subexpr, value)
      end
      
      # Causes the stack to evaluate expressions in order to pop them off the
      # stack, until it gets down to the size given by +limit+. The resulting
      # value if returned after all necessary computations have been done,
      # and if an error takes place at any point we empty the stack.
      def clear!(limit = 0)
        process! while size > limit
        @value
      rescue Exception => ex
        restack!
        raise ex
      end
      
      # Sets the +value+ on the +Stack+, which is always the value returned by
      # the last completed expression or function body. If the given +value+
      # is another +Stack+, this new stack replaces the state of the receiver;
      # this takes place when a +Continuation+ is called. If the +value+ is
      # a +Frame+, it is pushed onto the stack and we set a flag to indicate
      # that a tail call is in effect and the replacement target of the call
      # needs to be repointed: the expression that generated the tail call will
      # have been removed from the stack by the time the call returns.
      def value=(value)
        @value  = value
        @unwind = (Stack === @value)
        @tail   = (Frame === @value)
        restack!(value) if @unwind
      end
      
    private
      
      # Processes one piece of the final +Frame+ on the +Stack+ and inspects the
      # return value. The value must be inspected to see if a +Continuation+ has
      # been called (indicated by <tt>@unwind</tt>), or a tail call has taken
      # place. Continuation calls replace the state of the stack, and tail calls
      # need modifying so they fill the correct hole when they return.
      def process!
        self.value = last.process!
        return if empty? or @unwind or not last.complete?
        @value.replaces(last.target) if @tail
        fill!(pop.target, @value)
      end
      
      # Replaces the state of the receiver with the state of the argument. We
      # call this when calling a +Continuation+, or when recovering from errors.
      def restack!(stack = [])
        pop while not empty?
        stack.each_with_index { |frame, i| self[i] = frame }
        @value = stack.value if Stack === stack
      end
    end
    
  end
end

