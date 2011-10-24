module Heist
  module Scheme #:nodoc:
    include Treetop::Runtime

    def root
      @root || :program
    end

    module Program0 #:nodoc:
    end

    def _nt_program
      start_index = index
      if node_cache[:program].has_key?(index)
        cached = node_cache[:program][index]
        @index = cached.interval.end if cached
        return cached
      end

      i0, s0 = index, []
      r2 = _nt_shebang
      if r2
        r1 = r2
      else
        r1 = instantiate_node(SyntaxNode,input, index...index)
      end
      s0 << r1
      if r1
        s3, i3 = [], index
        loop do
          r4 = _nt_cell
          if r4
            s3 << r4
          else
            break
          end
        end
        r3 = instantiate_node(SyntaxNode,input, i3...index, s3)
        s0 << r3
      end
      if s0.last
        r0 = instantiate_node(Program,input, i0...index, s0)
        r0.extend(Program0)
      else
        self.index = i0
        r0 = nil
      end

      node_cache[:program][start_index] = r0

      return r0
    end

    module Shebang0 #:nodoc:
    end

    module Shebang1 #:nodoc:
    end

    def _nt_shebang
      start_index = index
      if node_cache[:shebang].has_key?(index)
        cached = node_cache[:shebang][index]
        @index = cached.interval.end if cached
        return cached
      end

      i0, s0 = index, []
      s1, i1 = [], index
      loop do
        r2 = _nt_space
        if r2
          s1 << r2
        else
          break
        end
      end
      r1 = instantiate_node(SyntaxNode,input, i1...index, s1)
      s0 << r1
      if r1
        if input.index("#!", index) == index
          r3 = instantiate_node(SyntaxNode,input, index...(index + 2))
          @index += 2
        else
          terminal_parse_failure("#!")
          r3 = nil
        end
        s0 << r3
        if r3
          s4, i4 = [], index
          loop do
            i5, s5 = index, []
            i6 = index
            if input.index(Regexp.new('[\\n\\r]'), index) == index
              r7 = instantiate_node(SyntaxNode,input, index...(index + 1))
              @index += 1
            else
              r7 = nil
            end
            if r7
              r6 = nil
            else
              self.index = i6
              r6 = instantiate_node(SyntaxNode,input, index...index)
            end
            s5 << r6
            if r6
              if index < input_length
                r8 = instantiate_node(SyntaxNode,input, index...(index + 1))
                @index += 1
              else
                terminal_parse_failure("any character")
                r8 = nil
              end
              s5 << r8
            end
            if s5.last
              r5 = instantiate_node(SyntaxNode,input, i5...index, s5)
              r5.extend(Shebang0)
            else
              self.index = i5
              r5 = nil
            end
            if r5
              s4 << r5
            else
              break
            end
          end
          r4 = instantiate_node(SyntaxNode,input, i4...index, s4)
          s0 << r4
        end
      end
      if s0.last
        r0 = instantiate_node(SyntaxNode,input, i0...index, s0)
        r0.extend(Shebang1)
      else
        self.index = i0
        r0 = nil
      end

      node_cache[:shebang][start_index] = r0

      return r0
    end

    module Cell0 #:nodoc:
      def ignore
        elements[0]
      end

      def quote
        elements[1]
      end

      def cell
        elements[2]
      end
    end

    module Cell1 #:nodoc:
      def ignore
        elements[0]
      end

      def ignore
        elements[2]
      end
    end

    def _nt_cell
      start_index = index
      if node_cache[:cell].has_key?(index)
        cached = node_cache[:cell][index]
        @index = cached.interval.end if cached
        return cached
      end

      i0 = index
      i1, s1 = index, []
      r2 = _nt_ignore
      s1 << r2
      if r2
        r3 = _nt_quote
        s1 << r3
        if r3
          r4 = _nt_cell
          s1 << r4
        end
      end
      if s1.last
        r1 = instantiate_node(QuotedCell,input, i1...index, s1)
        r1.extend(Cell0)
      else
        self.index = i1
        r1 = nil
      end
      if r1
        r0 = r1
      else
        i5, s5 = index, []
        r6 = _nt_ignore
        s5 << r6
        if r6
          i7 = index
          r8 = _nt_list
          if r8
            r7 = r8
          else
            r9 = _nt_vector
            if r9
              r7 = r9
            else
              r10 = _nt_atom
              if r10
                r7 = r10
              else
                self.index = i7
                r7 = nil
              end
            end
          end
          s5 << r7
          if r7
            r11 = _nt_ignore
            s5 << r11
          end
        end
        if s5.last
          r5 = instantiate_node(Cell,input, i5...index, s5)
          r5.extend(Cell1)
        else
          self.index = i5
          r5 = nil
        end
        if r5
          r0 = r5
        else
          self.index = i0
          r0 = nil
        end
      end

      node_cache[:cell][start_index] = r0

      return r0
    end

    def _nt_quote
      start_index = index
      if node_cache[:quote].has_key?(index)
        cached = node_cache[:quote][index]
        @index = cached.interval.end if cached
        return cached
      end

      i0 = index
      if input.index("'", index) == index
        r1 = instantiate_node(SyntaxNode,input, index...(index + 1))
        @index += 1
      else
        terminal_parse_failure("'")
        r1 = nil
      end
      if r1
        r0 = r1
      else
        if input.index("`", index) == index
          r2 = instantiate_node(SyntaxNode,input, index...(index + 1))
          @index += 1
        else
          terminal_parse_failure("`")
          r2 = nil
        end
        if r2
          r0 = r2
        else
          if input.index(",@", index) == index
            r3 = instantiate_node(SyntaxNode,input, index...(index + 2))
            @index += 2
          else
            terminal_parse_failure(",@")
            r3 = nil
          end
          if r3
            r0 = r3
          else
            if input.index(",", index) == index
              r4 = instantiate_node(SyntaxNode,input, index...(index + 1))
              @index += 1
            else
              terminal_parse_failure(",")
              r4 = nil
            end
            if r4
              r0 = r4
            else
              self.index = i0
              r0 = nil
            end
          end
        end
      end

      node_cache[:quote][start_index] = r0

      return r0
    end

    def _nt_dot
      start_index = index
      if node_cache[:dot].has_key?(index)
        cached = node_cache[:dot][index]
        @index = cached.interval.end if cached
        return cached
      end

      if input.index(".", index) == index
        r0 = instantiate_node(SyntaxNode,input, index...(index + 1))
        @index += 1
      else
        terminal_parse_failure(".")
        r0 = nil
      end

      node_cache[:dot][start_index] = r0

      return r0
    end

    def _nt_hash
      start_index = index
      if node_cache[:hash].has_key?(index)
        cached = node_cache[:hash][index]
        @index = cached.interval.end if cached
        return cached
      end

      if input.index("#", index) == index
        r0 = instantiate_node(SyntaxNode,input, index...(index + 1))
        @index += 1
      else
        terminal_parse_failure("#")
        r0 = nil
      end

      node_cache[:hash][start_index] = r0

      return r0
    end

    module List0 #:nodoc:
      def cells
        elements[1]
      end

    end

    module List1 #:nodoc:
      def cells
        elements[1]
      end

    end

    def _nt_list
      start_index = index
      if node_cache[:list].has_key?(index)
        cached = node_cache[:list][index]
        @index = cached.interval.end if cached
        return cached
      end

      i0 = index
      i1, s1 = index, []
      if input.index("(", index) == index
        r2 = instantiate_node(SyntaxNode,input, index...(index + 1))
        @index += 1
      else
        terminal_parse_failure("(")
        r2 = nil
      end
      s1 << r2
      if r2
        r3 = _nt_cells
        s1 << r3
        if r3
          if input.index(")", index) == index
            r4 = instantiate_node(SyntaxNode,input, index...(index + 1))
            @index += 1
          else
            terminal_parse_failure(")")
            r4 = nil
          end
          s1 << r4
        end
      end
      if s1.last
        r1 = instantiate_node(SyntaxNode,input, i1...index, s1)
        r1.extend(List0)
      else
        self.index = i1
        r1 = nil
      end
      if r1
        r0 = r1
        r0.extend(List)
      else
        i5, s5 = index, []
        if input.index("[", index) == index
          r6 = instantiate_node(SyntaxNode,input, index...(index + 1))
          @index += 1
        else
          terminal_parse_failure("[")
          r6 = nil
        end
        s5 << r6
        if r6
          r7 = _nt_cells
          s5 << r7
          if r7
            if input.index("]", index) == index
              r8 = instantiate_node(SyntaxNode,input, index...(index + 1))
              @index += 1
            else
              terminal_parse_failure("]")
              r8 = nil
            end
            s5 << r8
          end
        end
        if s5.last
          r5 = instantiate_node(SyntaxNode,input, i5...index, s5)
          r5.extend(List1)
        else
          self.index = i5
          r5 = nil
        end
        if r5
          r0 = r5
          r0.extend(List)
        else
          self.index = i0
          r0 = nil
        end
      end

      node_cache[:list][start_index] = r0

      return r0
    end

    module Cells0 #:nodoc:
      def dot
        elements[0]
      end

      def space
        elements[1]
      end

      def cell
        elements[2]
      end
    end

    module Cells1 #:nodoc:
    end

    module Cells2 #:nodoc:
      def ignore
        elements[1]
      end
    end

    def _nt_cells
      start_index = index
      if node_cache[:cells].has_key?(index)
        cached = node_cache[:cells][index]
        @index = cached.interval.end if cached
        return cached
      end

      i0 = index
      i1, s1 = index, []
      s2, i2 = [], index
      loop do
        r3 = _nt_cell
        if r3
          s2 << r3
        else
          break
        end
      end
      if s2.empty?
        self.index = i2
        r2 = nil
      else
        r2 = instantiate_node(SyntaxNode,input, i2...index, s2)
      end
      s1 << r2
      if r2
        i4, s4 = index, []
        r5 = _nt_dot
        s4 << r5
        if r5
          r6 = _nt_space
          s4 << r6
          if r6
            r7 = _nt_cell
            s4 << r7
          end
        end
        if s4.last
          r4 = instantiate_node(SyntaxNode,input, i4...index, s4)
          r4.extend(Cells0)
        else
          self.index = i4
          r4 = nil
        end
        s1 << r4
      end
      if s1.last
        r1 = instantiate_node(SyntaxNode,input, i1...index, s1)
        r1.extend(Cells1)
      else
        self.index = i1
        r1 = nil
      end
      if r1
        r0 = r1
      else
        i8, s8 = index, []
        s9, i9 = [], index
        loop do
          r10 = _nt_cell
          if r10
            s9 << r10
          else
            break
          end
        end
        r9 = instantiate_node(SyntaxNode,input, i9...index, s9)
        s8 << r9
        if r9
          r11 = _nt_ignore
          s8 << r11
        end
        if s8.last
          r8 = instantiate_node(SyntaxNode,input, i8...index, s8)
          r8.extend(Cells2)
        else
          self.index = i8
          r8 = nil
        end
        if r8
          r0 = r8
        else
          self.index = i0
          r0 = nil
        end
      end

      node_cache[:cells][start_index] = r0

      return r0
    end

    module Vector0 #:nodoc:
      def hash
        elements[0]
      end

      def ignore
        elements[3]
      end

    end

    module Vector1 #:nodoc:
      def hash
        elements[0]
      end

      def ignore
        elements[3]
      end

    end

    def _nt_vector
      start_index = index
      if node_cache[:vector].has_key?(index)
        cached = node_cache[:vector][index]
        @index = cached.interval.end if cached
        return cached
      end

      i0 = index
      i1, s1 = index, []
      r2 = _nt_hash
      s1 << r2
      if r2
        if input.index("(", index) == index
          r3 = instantiate_node(SyntaxNode,input, index...(index + 1))
          @index += 1
        else
          terminal_parse_failure("(")
          r3 = nil
        end
        s1 << r3
        if r3
          s4, i4 = [], index
          loop do
            r5 = _nt_cell
            if r5
              s4 << r5
            else
              break
            end
          end
          r4 = instantiate_node(SyntaxNode,input, i4...index, s4)
          s1 << r4
          if r4
            r6 = _nt_ignore
            s1 << r6
            if r6
              if input.index(")", index) == index
                r7 = instantiate_node(SyntaxNode,input, index...(index + 1))
                @index += 1
              else
                terminal_parse_failure(")")
                r7 = nil
              end
              s1 << r7
            end
          end
        end
      end
      if s1.last
        r1 = instantiate_node(SyntaxNode,input, i1...index, s1)
        r1.extend(Vector0)
      else
        self.index = i1
        r1 = nil
      end
      if r1
        r0 = r1
        r0.extend(Vector)
      else
        i8, s8 = index, []
        r9 = _nt_hash
        s8 << r9
        if r9
          if input.index("[", index) == index
            r10 = instantiate_node(SyntaxNode,input, index...(index + 1))
            @index += 1
          else
            terminal_parse_failure("[")
            r10 = nil
          end
          s8 << r10
          if r10
            s11, i11 = [], index
            loop do
              r12 = _nt_cell
              if r12
                s11 << r12
              else
                break
              end
            end
            r11 = instantiate_node(SyntaxNode,input, i11...index, s11)
            s8 << r11
            if r11
              r13 = _nt_ignore
              s8 << r13
              if r13
                if input.index("]", index) == index
                  r14 = instantiate_node(SyntaxNode,input, index...(index + 1))
                  @index += 1
                else
                  terminal_parse_failure("]")
                  r14 = nil
                end
                s8 << r14
              end
            end
          end
        end
        if s8.last
          r8 = instantiate_node(SyntaxNode,input, i8...index, s8)
          r8.extend(Vector1)
        else
          self.index = i8
          r8 = nil
        end
        if r8
          r0 = r8
          r0.extend(Vector)
        else
          self.index = i0
          r0 = nil
        end
      end

      node_cache[:vector][start_index] = r0

      return r0
    end

    def _nt_atom
      start_index = index
      if node_cache[:atom].has_key?(index)
        cached = node_cache[:atom][index]
        @index = cached.interval.end if cached
        return cached
      end

      i0 = index
      r1 = _nt_datum
      if r1
        r0 = r1
      else
        r2 = _nt_identifier
        if r2
          r0 = r2
        else
          self.index = i0
          r0 = nil
        end
      end

      node_cache[:atom][start_index] = r0

      return r0
    end

    module Datum0 #:nodoc:
    end

    module Datum1 #:nodoc:
    end

    def _nt_datum
      start_index = index
      if node_cache[:datum].has_key?(index)
        cached = node_cache[:datum][index]
        @index = cached.interval.end if cached
        return cached
      end

      i0, s0 = index, []
      i1 = index
      r2 = _nt_boolean
      if r2
        r1 = r2
      else
        r3 = _nt_number
        if r3
          r1 = r3
        else
          r4 = _nt_character
          if r4
            r1 = r4
          else
            r5 = _nt_string
            if r5
              r1 = r5
            else
              self.index = i1
              r1 = nil
            end
          end
        end
      end
      s0 << r1
      if r1
        i6 = index
        i7, s7 = index, []
        i8 = index
        r9 = _nt_delimiter
        if r9
          r8 = nil
        else
          self.index = i8
          r8 = instantiate_node(SyntaxNode,input, index...index)
        end
        s7 << r8
        if r8
          if index < input_length
            r10 = instantiate_node(SyntaxNode,input, index...(index + 1))
            @index += 1
          else
            terminal_parse_failure("any character")
            r10 = nil
          end
          s7 << r10
        end
        if s7.last
          r7 = instantiate_node(SyntaxNode,input, i7...index, s7)
          r7.extend(Datum0)
        else
          self.index = i7
          r7 = nil
        end
        if r7
          r6 = nil
        else
          self.index = i6
          r6 = instantiate_node(SyntaxNode,input, index...index)
        end
        s0 << r6
      end
      if s0.last
        r0 = instantiate_node(Datum,input, i0...index, s0)
        r0.extend(Datum1)
      else
        self.index = i0
        r0 = nil
      end

      node_cache[:datum][start_index] = r0

      return r0
    end

    module Boolean0 #:nodoc:
      def hash
        elements[0]
      end

    end

    def _nt_boolean
      start_index = index
      if node_cache[:boolean].has_key?(index)
        cached = node_cache[:boolean][index]
        @index = cached.interval.end if cached
        return cached
      end

      i0, s0 = index, []
      r1 = _nt_hash
      s0 << r1
      if r1
        if input.index(Regexp.new('[tf]'), index) == index
          r2 = instantiate_node(SyntaxNode,input, index...(index + 1))
          @index += 1
        else
          r2 = nil
        end
        s0 << r2
      end
      if s0.last
        r0 = instantiate_node(Boolean,input, i0...index, s0)
        r0.extend(Boolean0)
      else
        self.index = i0
        r0 = nil
      end

      node_cache[:boolean][start_index] = r0

      return r0
    end

    def _nt_number
      start_index = index
      if node_cache[:number].has_key?(index)
        cached = node_cache[:number][index]
        @index = cached.interval.end if cached
        return cached
      end

      i0 = index
      r1 = _nt_complex
      if r1
        r0 = r1
      else
        r2 = _nt_real
        if r2
          r0 = r2
        else
          r3 = _nt_rational
          if r3
            r0 = r3
          else
            r4 = _nt_integer
            if r4
              r0 = r4
            else
              self.index = i0
              r0 = nil
            end
          end
        end
      end

      node_cache[:number][start_index] = r0

      return r0
    end

    module Complex0 #:nodoc:
      def real
        elements[0]
      end

      def imaginary
        elements[2]
      end

    end

    def _nt_complex
      start_index = index
      if node_cache[:complex].has_key?(index)
        cached = node_cache[:complex][index]
        @index = cached.interval.end if cached
        return cached
      end

      i0, s0 = index, []
      i1 = index
      r2 = _nt_real
      if r2
        r1 = r2
      else
        r3 = _nt_integer
        if r3
          r1 = r3
        else
          self.index = i1
          r1 = nil
        end
      end
      s0 << r1
      if r1
        if input.index("+", index) == index
          r4 = instantiate_node(SyntaxNode,input, index...(index + 1))
          @index += 1
        else
          terminal_parse_failure("+")
          r4 = nil
        end
        s0 << r4
        if r4
          i5 = index
          r6 = _nt_real
          if r6
            r5 = r6
          else
            r7 = _nt_integer
            if r7
              r5 = r7
            else
              self.index = i5
              r5 = nil
            end
          end
          s0 << r5
          if r5
            if input.index("i", index) == index
              r8 = instantiate_node(SyntaxNode,input, index...(index + 1))
              @index += 1
            else
              terminal_parse_failure("i")
              r8 = nil
            end
            s0 << r8
          end
        end
      end
      if s0.last
        r0 = instantiate_node(Complex,input, i0...index, s0)
        r0.extend(Complex0)
      else
        self.index = i0
        r0 = nil
      end

      node_cache[:complex][start_index] = r0

      return r0
    end

    module Real0 #:nodoc:
    end

    module Real1 #:nodoc:
      def integer
        elements[0]
      end

    end

    def _nt_real
      start_index = index
      if node_cache[:real].has_key?(index)
        cached = node_cache[:real][index]
        @index = cached.interval.end if cached
        return cached
      end

      i0, s0 = index, []
      r1 = _nt_integer
      s0 << r1
      if r1
        i2, s2 = index, []
        if input.index(".", index) == index
          r3 = instantiate_node(SyntaxNode,input, index...(index + 1))
          @index += 1
        else
          terminal_parse_failure(".")
          r3 = nil
        end
        s2 << r3
        if r3
          s4, i4 = [], index
          loop do
            r5 = _nt_digit
            if r5
              s4 << r5
            else
              break
            end
          end
          if s4.empty?
            self.index = i4
            r4 = nil
          else
            r4 = instantiate_node(SyntaxNode,input, i4...index, s4)
          end
          s2 << r4
        end
        if s2.last
          r2 = instantiate_node(SyntaxNode,input, i2...index, s2)
          r2.extend(Real0)
        else
          self.index = i2
          r2 = nil
        end
        s0 << r2
      end
      if s0.last
        r0 = instantiate_node(Real,input, i0...index, s0)
        r0.extend(Real1)
      else
        self.index = i0
        r0 = nil
      end

      node_cache[:real][start_index] = r0

      return r0
    end

    module Rational0 #:nodoc:
      def numerator
        elements[0]
      end

      def denominator
        elements[2]
      end
    end

    def _nt_rational
      start_index = index
      if node_cache[:rational].has_key?(index)
        cached = node_cache[:rational][index]
        @index = cached.interval.end if cached
        return cached
      end

      i0, s0 = index, []
      r1 = _nt_integer
      s0 << r1
      if r1
        if input.index("/", index) == index
          r2 = instantiate_node(SyntaxNode,input, index...(index + 1))
          @index += 1
        else
          terminal_parse_failure("/")
          r2 = nil
        end
        s0 << r2
        if r2
          r3 = _nt_integer
          s0 << r3
        end
      end
      if s0.last
        r0 = instantiate_node(Rational,input, i0...index, s0)
        r0.extend(Rational0)
      else
        self.index = i0
        r0 = nil
      end

      node_cache[:rational][start_index] = r0

      return r0
    end

    module Integer0 #:nodoc:
    end

    module Integer1 #:nodoc:
    end

    def _nt_integer
      start_index = index
      if node_cache[:integer].has_key?(index)
        cached = node_cache[:integer][index]
        @index = cached.interval.end if cached
        return cached
      end

      i0, s0 = index, []
      if input.index("-", index) == index
        r2 = instantiate_node(SyntaxNode,input, index...(index + 1))
        @index += 1
      else
        terminal_parse_failure("-")
        r2 = nil
      end
      if r2
        r1 = r2
      else
        r1 = instantiate_node(SyntaxNode,input, index...index)
      end
      s0 << r1
      if r1
        i3 = index
        if input.index("0", index) == index
          r4 = instantiate_node(SyntaxNode,input, index...(index + 1))
          @index += 1
        else
          terminal_parse_failure("0")
          r4 = nil
        end
        if r4
          r3 = r4
        else
          i5, s5 = index, []
          if input.index(Regexp.new('[1-9]'), index) == index
            r6 = instantiate_node(SyntaxNode,input, index...(index + 1))
            @index += 1
          else
            r6 = nil
          end
          s5 << r6
          if r6
            s7, i7 = [], index
            loop do
              r8 = _nt_digit
              if r8
                s7 << r8
              else
                break
              end
            end
            r7 = instantiate_node(SyntaxNode,input, i7...index, s7)
            s5 << r7
          end
          if s5.last
            r5 = instantiate_node(SyntaxNode,input, i5...index, s5)
            r5.extend(Integer0)
          else
            self.index = i5
            r5 = nil
          end
          if r5
            r3 = r5
          else
            self.index = i3
            r3 = nil
          end
        end
        s0 << r3
      end
      if s0.last
        r0 = instantiate_node(Integer,input, i0...index, s0)
        r0.extend(Integer1)
      else
        self.index = i0
        r0 = nil
      end

      node_cache[:integer][start_index] = r0

      return r0
    end

    module Character0 #:nodoc:
      def glyph
        elements[1]
      end
    end

    def _nt_character
      start_index = index
      if node_cache[:character].has_key?(index)
        cached = node_cache[:character][index]
        @index = cached.interval.end if cached
        return cached
      end

      i0, s0 = index, []
      if input.index("#\\", index) == index
        r1 = instantiate_node(SyntaxNode,input, index...(index + 2))
        @index += 2
      else
        terminal_parse_failure("#\\")
        r1 = nil
      end
      s0 << r1
      if r1
        i2 = index
        r3 = _nt_identifier
        if r3
          r2 = r3
        else
          if index < input_length
            r4 = instantiate_node(SyntaxNode,input, index...(index + 1))
            @index += 1
          else
            terminal_parse_failure("any character")
            r4 = nil
          end
          if r4
            r2 = r4
          else
            self.index = i2
            r2 = nil
          end
        end
        s0 << r2
      end
      if s0.last
        r0 = instantiate_node(Character,input, i0...index, s0)
        r0.extend(Character0)
      else
        self.index = i0
        r0 = nil
      end

      node_cache[:character][start_index] = r0

      return r0
    end

    module String0 #:nodoc:
    end

    def _nt_string
      start_index = index
      if node_cache[:string].has_key?(index)
        cached = node_cache[:string][index]
        @index = cached.interval.end if cached
        return cached
      end

      i0, s0 = index, []
      if input.index('"', index) == index
        r1 = instantiate_node(SyntaxNode,input, index...(index + 1))
        @index += 1
      else
        terminal_parse_failure('"')
        r1 = nil
      end
      s0 << r1
      if r1
        s2, i2 = [], index
        loop do
          i3 = index
          if input.index('\\"', index) == index
            r4 = instantiate_node(SyntaxNode,input, index...(index + 2))
            @index += 2
          else
            terminal_parse_failure('\\"')
            r4 = nil
          end
          if r4
            r3 = r4
          else
            if input.index(Regexp.new('[^"]'), index) == index
              r5 = instantiate_node(SyntaxNode,input, index...(index + 1))
              @index += 1
            else
              r5 = nil
            end
            if r5
              r3 = r5
            else
              self.index = i3
              r3 = nil
            end
          end
          if r3
            s2 << r3
          else
            break
          end
        end
        r2 = instantiate_node(SyntaxNode,input, i2...index, s2)
        s0 << r2
        if r2
          if input.index('"', index) == index
            r6 = instantiate_node(SyntaxNode,input, index...(index + 1))
            @index += 1
          else
            terminal_parse_failure('"')
            r6 = nil
          end
          s0 << r6
        end
      end
      if s0.last
        r0 = instantiate_node(String,input, i0...index, s0)
        r0.extend(String0)
      else
        self.index = i0
        r0 = nil
      end

      node_cache[:string][start_index] = r0

      return r0
    end

    module Identifier0 #:nodoc:
    end

    module Identifier1 #:nodoc:
    end

    module Identifier2 #:nodoc:
    end

    module Identifier3 #:nodoc:
    end

    def _nt_identifier
      start_index = index
      if node_cache[:identifier].has_key?(index)
        cached = node_cache[:identifier][index]
        @index = cached.interval.end if cached
        return cached
      end

      i0 = index
      i1, s1 = index, []
      i2, s2 = index, []
      i3 = index
      r4 = _nt_delimiter
      if r4
        r3 = nil
      else
        self.index = i3
        r3 = instantiate_node(SyntaxNode,input, index...index)
      end
      s2 << r3
      if r3
        if index < input_length
          r5 = instantiate_node(SyntaxNode,input, index...(index + 1))
          @index += 1
        else
          terminal_parse_failure("any character")
          r5 = nil
        end
        s2 << r5
      end
      if s2.last
        r2 = instantiate_node(SyntaxNode,input, i2...index, s2)
        r2.extend(Identifier0)
      else
        self.index = i2
        r2 = nil
      end
      s1 << r2
      if r2
        s6, i6 = [], index
        loop do
          i7, s7 = index, []
          i8 = index
          r9 = _nt_delimiter
          if r9
            r8 = nil
          else
            self.index = i8
            r8 = instantiate_node(SyntaxNode,input, index...index)
          end
          s7 << r8
          if r8
            if index < input_length
              r10 = instantiate_node(SyntaxNode,input, index...(index + 1))
              @index += 1
            else
              terminal_parse_failure("any character")
              r10 = nil
            end
            s7 << r10
          end
          if s7.last
            r7 = instantiate_node(SyntaxNode,input, i7...index, s7)
            r7.extend(Identifier1)
          else
            self.index = i7
            r7 = nil
          end
          if r7
            s6 << r7
          else
            break
          end
        end
        if s6.empty?
          self.index = i6
          r6 = nil
        else
          r6 = instantiate_node(SyntaxNode,input, i6...index, s6)
        end
        s1 << r6
      end
      if s1.last
        r1 = instantiate_node(SyntaxNode,input, i1...index, s1)
        r1.extend(Identifier2)
      else
        self.index = i1
        r1 = nil
      end
      if r1
        r0 = r1
        r0.extend(Identifier)
      else
        i11, s11 = index, []
        i12 = index
        r13 = _nt_reserved
        if r13
          r12 = nil
        else
          self.index = i12
          r12 = instantiate_node(SyntaxNode,input, index...index)
        end
        s11 << r12
        if r12
          if index < input_length
            r14 = instantiate_node(SyntaxNode,input, index...(index + 1))
            @index += 1
          else
            terminal_parse_failure("any character")
            r14 = nil
          end
          s11 << r14
        end
        if s11.last
          r11 = instantiate_node(SyntaxNode,input, i11...index, s11)
          r11.extend(Identifier3)
        else
          self.index = i11
          r11 = nil
        end
        if r11
          r0 = r11
          r0.extend(Identifier)
        else
          self.index = i0
          r0 = nil
        end
      end

      node_cache[:identifier][start_index] = r0

      return r0
    end

    def _nt_digit
      start_index = index
      if node_cache[:digit].has_key?(index)
        cached = node_cache[:digit][index]
        @index = cached.interval.end if cached
        return cached
      end

      if input.index(Regexp.new('[0-9]'), index) == index
        r0 = instantiate_node(SyntaxNode,input, index...(index + 1))
        @index += 1
      else
        r0 = nil
      end

      node_cache[:digit][start_index] = r0

      return r0
    end

    def _nt_reserved
      start_index = index
      if node_cache[:reserved].has_key?(index)
        cached = node_cache[:reserved][index]
        @index = cached.interval.end if cached
        return cached
      end

      i0 = index
      r1 = _nt_dot
      if r1
        r0 = r1
      else
        r2 = _nt_delimiter
        if r2
          r0 = r2
        else
          self.index = i0
          r0 = nil
        end
      end

      node_cache[:reserved][start_index] = r0

      return r0
    end

    def _nt_delimiter
      start_index = index
      if node_cache[:delimiter].has_key?(index)
        cached = node_cache[:delimiter][index]
        @index = cached.interval.end if cached
        return cached
      end

      i0 = index
      r1 = _nt_quote
      if r1
        r0 = r1
      else
        r2 = _nt_hash
        if r2
          r0 = r2
        else
          r3 = _nt_paren
          if r3
            r0 = r3
          else
            r4 = _nt_space
            if r4
              r0 = r4
            else
              self.index = i0
              r0 = nil
            end
          end
        end
      end

      node_cache[:delimiter][start_index] = r0

      return r0
    end

    def _nt_paren
      start_index = index
      if node_cache[:paren].has_key?(index)
        cached = node_cache[:paren][index]
        @index = cached.interval.end if cached
        return cached
      end

      if input.index(Regexp.new('[\\(\\)\\[\\]]'), index) == index
        r0 = instantiate_node(SyntaxNode,input, index...(index + 1))
        @index += 1
      else
        r0 = nil
      end

      node_cache[:paren][start_index] = r0

      return r0
    end

    def _nt_space
      start_index = index
      if node_cache[:space].has_key?(index)
        cached = node_cache[:space][index]
        @index = cached.interval.end if cached
        return cached
      end

      if input.index(Regexp.new('[\\s\\n\\r\\t]'), index) == index
        r0 = instantiate_node(SyntaxNode,input, index...(index + 1))
        @index += 1
      else
        r0 = nil
      end

      node_cache[:space][start_index] = r0

      return r0
    end

    module Ignore0 #:nodoc:
      def comment
        elements[0]
      end

      def ignore
        elements[1]
      end
    end

    module Ignore1 #:nodoc:
    end

    def _nt_ignore
      start_index = index
      if node_cache[:ignore].has_key?(index)
        cached = node_cache[:ignore][index]
        @index = cached.interval.end if cached
        return cached
      end

      i0, s0 = index, []
      s1, i1 = [], index
      loop do
        r2 = _nt_space
        if r2
          s1 << r2
        else
          break
        end
      end
      r1 = instantiate_node(SyntaxNode,input, i1...index, s1)
      s0 << r1
      if r1
        i4, s4 = index, []
        r5 = _nt_comment
        s4 << r5
        if r5
          r6 = _nt_ignore
          s4 << r6
        end
        if s4.last
          r4 = instantiate_node(SyntaxNode,input, i4...index, s4)
          r4.extend(Ignore0)
        else
          self.index = i4
          r4 = nil
        end
        if r4
          r3 = r4
        else
          r3 = instantiate_node(SyntaxNode,input, index...index)
        end
        s0 << r3
      end
      if s0.last
        r0 = instantiate_node(SyntaxNode,input, i0...index, s0)
        r0.extend(Ignore1)
      else
        self.index = i0
        r0 = nil
      end

      node_cache[:ignore][start_index] = r0

      return r0
    end

    module Comment0 #:nodoc:
    end

    module Comment1 #:nodoc:
    end

    def _nt_comment
      start_index = index
      if node_cache[:comment].has_key?(index)
        cached = node_cache[:comment][index]
        @index = cached.interval.end if cached
        return cached
      end

      i0, s0 = index, []
      if input.index(";", index) == index
        r1 = instantiate_node(SyntaxNode,input, index...(index + 1))
        @index += 1
      else
        terminal_parse_failure(";")
        r1 = nil
      end
      s0 << r1
      if r1
        s2, i2 = [], index
        loop do
          i3, s3 = index, []
          i4 = index
          if input.index(Regexp.new('[\\n\\r]'), index) == index
            r5 = instantiate_node(SyntaxNode,input, index...(index + 1))
            @index += 1
          else
            r5 = nil
          end
          if r5
            r4 = nil
          else
            self.index = i4
            r4 = instantiate_node(SyntaxNode,input, index...index)
          end
          s3 << r4
          if r4
            if index < input_length
              r6 = instantiate_node(SyntaxNode,input, index...(index + 1))
              @index += 1
            else
              terminal_parse_failure("any character")
              r6 = nil
            end
            s3 << r6
          end
          if s3.last
            r3 = instantiate_node(SyntaxNode,input, i3...index, s3)
            r3.extend(Comment0)
          else
            self.index = i3
            r3 = nil
          end
          if r3
            s2 << r3
          else
            break
          end
        end
        r2 = instantiate_node(SyntaxNode,input, i2...index, s2)
        s0 << r2
      end
      if s0.last
        r0 = instantiate_node(SyntaxNode,input, i0...index, s0)
        r0.extend(Comment1)
      else
        self.index = i0
        r0 = nil
      end

      node_cache[:comment][start_index] = r0

      return r0
    end

  end

  class SchemeParser < Treetop::Runtime::CompiledParser
    include Scheme
  end

end

