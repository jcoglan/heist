module Heist
  module Scheme
    include Treetop::Runtime

    def root
      @root || :program
    end

    def _nt_program
      start_index = index
      if node_cache[:program].has_key?(index)
        cached = node_cache[:program][index]
        @index = cached.interval.end if cached
        return cached
      end

      s0, i0 = [], index
      loop do
        r1 = _nt_cell
        if r1
          s0 << r1
        else
          break
        end
      end
      r0 = Program.new(input, i0...index, s0)

      node_cache[:program][start_index] = r0

      return r0
    end

    module Cell0
      def space
        elements[0]
      end

      def space
        elements[2]
      end

      def space
        elements[4]
      end
    end

    def _nt_cell
      start_index = index
      if node_cache[:cell].has_key?(index)
        cached = node_cache[:cell][index]
        @index = cached.interval.end if cached
        return cached
      end

      i0, s0 = index, []
      r1 = _nt_space
      s0 << r1
      if r1
        r3 = _nt_quote
        if r3
          r2 = r3
        else
          r2 = SyntaxNode.new(input, index...index)
        end
        s0 << r2
        if r2
          r4 = _nt_space
          s0 << r4
          if r4
            i5 = index
            r6 = _nt_list
            if r6
              r5 = r6
            else
              r7 = _nt_atom
              if r7
                r5 = r7
              else
                self.index = i5
                r5 = nil
              end
            end
            s0 << r5
            if r5
              r8 = _nt_space
              s0 << r8
            end
          end
        end
      end
      if s0.last
        r0 = (Cell).new(input, i0...index, s0)
        r0.extend(Cell0)
      else
        self.index = i0
        r0 = nil
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
        r1 = (SyntaxNode).new(input, index...(index + 1))
        @index += 1
      else
        terminal_parse_failure("'")
        r1 = nil
      end
      if r1
        r0 = r1
      else
        if input.index("`", index) == index
          r2 = (SyntaxNode).new(input, index...(index + 1))
          @index += 1
        else
          terminal_parse_failure("`")
          r2 = nil
        end
        if r2
          r0 = r2
        else
          if input.index(",@", index) == index
            r3 = (SyntaxNode).new(input, index...(index + 2))
            @index += 2
          else
            terminal_parse_failure(",@")
            r3 = nil
          end
          if r3
            r0 = r3
          else
            if input.index(",", index) == index
              r4 = (SyntaxNode).new(input, index...(index + 1))
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

    module List0
    end

    module List1
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
        r2 = (SyntaxNode).new(input, index...(index + 1))
        @index += 1
      else
        terminal_parse_failure("(")
        r2 = nil
      end
      s1 << r2
      if r2
        s3, i3 = [], index
        loop do
          r4 = _nt_cell
          if r4
            s3 << r4
          else
            break
          end
        end
        r3 = SyntaxNode.new(input, i3...index, s3)
        s1 << r3
        if r3
          if input.index(")", index) == index
            r5 = (SyntaxNode).new(input, index...(index + 1))
            @index += 1
          else
            terminal_parse_failure(")")
            r5 = nil
          end
          s1 << r5
        end
      end
      if s1.last
        r1 = (SyntaxNode).new(input, i1...index, s1)
        r1.extend(List0)
      else
        self.index = i1
        r1 = nil
      end
      if r1
        r0 = r1
        r0.extend(List)
      else
        i6, s6 = index, []
        if input.index("[", index) == index
          r7 = (SyntaxNode).new(input, index...(index + 1))
          @index += 1
        else
          terminal_parse_failure("[")
          r7 = nil
        end
        s6 << r7
        if r7
          s8, i8 = [], index
          loop do
            r9 = _nt_cell
            if r9
              s8 << r9
            else
              break
            end
          end
          r8 = SyntaxNode.new(input, i8...index, s8)
          s6 << r8
          if r8
            if input.index("]", index) == index
              r10 = (SyntaxNode).new(input, index...(index + 1))
              @index += 1
            else
              terminal_parse_failure("]")
              r10 = nil
            end
            s6 << r10
          end
        end
        if s6.last
          r6 = (SyntaxNode).new(input, i6...index, s6)
          r6.extend(List1)
        else
          self.index = i6
          r6 = nil
        end
        if r6
          r0 = r6
          r0.extend(List)
        else
          self.index = i0
          r0 = nil
        end
      end

      node_cache[:list][start_index] = r0

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

    def _nt_datum
      start_index = index
      if node_cache[:datum].has_key?(index)
        cached = node_cache[:datum][index]
        @index = cached.interval.end if cached
        return cached
      end

      i0 = index
      r1 = _nt_boolean
      if r1
        r0 = r1
      else
        r2 = _nt_number
        if r2
          r0 = r2
        else
          r3 = _nt_string
          if r3
            r0 = r3
          else
            self.index = i0
            r0 = nil
          end
        end
      end

      node_cache[:datum][start_index] = r0

      return r0
    end

    def _nt_boolean
      start_index = index
      if node_cache[:boolean].has_key?(index)
        cached = node_cache[:boolean][index]
        @index = cached.interval.end if cached
        return cached
      end

      i0 = index
      if input.index("#t", index) == index
        r1 = (SyntaxNode).new(input, index...(index + 2))
        @index += 2
      else
        terminal_parse_failure("#t")
        r1 = nil
      end
      if r1
        r0 = r1
        r0.extend(Boolean)
      else
        if input.index("#f", index) == index
          r2 = (SyntaxNode).new(input, index...(index + 2))
          @index += 2
        else
          terminal_parse_failure("#f")
          r2 = nil
        end
        if r2
          r0 = r2
          r0.extend(Boolean)
        else
          self.index = i0
          r0 = nil
        end
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

    module Complex0
      def real
        elements[0]
      end

      def real
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
      r1 = _nt_real
      s0 << r1
      if r1
        if input.index("+", index) == index
          r2 = (SyntaxNode).new(input, index...(index + 1))
          @index += 1
        else
          terminal_parse_failure("+")
          r2 = nil
        end
        s0 << r2
        if r2
          r3 = _nt_real
          s0 << r3
          if r3
            if input.index("i", index) == index
              r4 = (SyntaxNode).new(input, index...(index + 1))
              @index += 1
            else
              terminal_parse_failure("i")
              r4 = nil
            end
            s0 << r4
          end
        end
      end
      if s0.last
        r0 = (Complex).new(input, i0...index, s0)
        r0.extend(Complex0)
      else
        self.index = i0
        r0 = nil
      end

      node_cache[:complex][start_index] = r0

      return r0
    end

    module Real0
    end

    module Real1
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
          r3 = (SyntaxNode).new(input, index...(index + 1))
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
            r4 = SyntaxNode.new(input, i4...index, s4)
          end
          s2 << r4
        end
        if s2.last
          r2 = (SyntaxNode).new(input, i2...index, s2)
          r2.extend(Real0)
        else
          self.index = i2
          r2 = nil
        end
        s0 << r2
      end
      if s0.last
        r0 = (Real).new(input, i0...index, s0)
        r0.extend(Real1)
      else
        self.index = i0
        r0 = nil
      end

      node_cache[:real][start_index] = r0

      return r0
    end

    module Rational0
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
          r2 = (SyntaxNode).new(input, index...(index + 1))
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
        r0 = (Rational).new(input, i0...index, s0)
        r0.extend(Rational0)
      else
        self.index = i0
        r0 = nil
      end

      node_cache[:rational][start_index] = r0

      return r0
    end

    module Integer0
    end

    module Integer1
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
        r2 = (SyntaxNode).new(input, index...(index + 1))
        @index += 1
      else
        terminal_parse_failure("-")
        r2 = nil
      end
      if r2
        r1 = r2
      else
        r1 = SyntaxNode.new(input, index...index)
      end
      s0 << r1
      if r1
        i3 = index
        if input.index("0", index) == index
          r4 = (SyntaxNode).new(input, index...(index + 1))
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
            r6 = (SyntaxNode).new(input, index...(index + 1))
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
            r7 = SyntaxNode.new(input, i7...index, s7)
            s5 << r7
          end
          if s5.last
            r5 = (SyntaxNode).new(input, i5...index, s5)
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
        r0 = (Integer).new(input, i0...index, s0)
        r0.extend(Integer1)
      else
        self.index = i0
        r0 = nil
      end

      node_cache[:integer][start_index] = r0

      return r0
    end

    module String0
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
        r1 = (SyntaxNode).new(input, index...(index + 1))
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
            r4 = (SyntaxNode).new(input, index...(index + 2))
            @index += 2
          else
            terminal_parse_failure('\\"')
            r4 = nil
          end
          if r4
            r3 = r4
          else
            if input.index(Regexp.new('[^"]'), index) == index
              r5 = (SyntaxNode).new(input, index...(index + 1))
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
        r2 = SyntaxNode.new(input, i2...index, s2)
        s0 << r2
        if r2
          if input.index('"', index) == index
            r6 = (SyntaxNode).new(input, index...(index + 1))
            @index += 1
          else
            terminal_parse_failure('"')
            r6 = nil
          end
          s0 << r6
        end
      end
      if s0.last
        r0 = (String).new(input, i0...index, s0)
        r0.extend(String0)
      else
        self.index = i0
        r0 = nil
      end

      node_cache[:string][start_index] = r0

      return r0
    end

    def _nt_identifier
      start_index = index
      if node_cache[:identifier].has_key?(index)
        cached = node_cache[:identifier][index]
        @index = cached.interval.end if cached
        return cached
      end

      s0, i0 = [], index
      loop do
        if input.index(Regexp.new('[^\\(\\)\\[\\]\\s]'), index) == index
          r1 = (SyntaxNode).new(input, index...(index + 1))
          @index += 1
        else
          r1 = nil
        end
        if r1
          s0 << r1
        else
          break
        end
      end
      if s0.empty?
        self.index = i0
        r0 = nil
      else
        r0 = Identifier.new(input, i0...index, s0)
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
        r0 = (SyntaxNode).new(input, index...(index + 1))
        @index += 1
      else
        r0 = nil
      end

      node_cache[:digit][start_index] = r0

      return r0
    end

    module Space0
    end

    def _nt_space
      start_index = index
      if node_cache[:space].has_key?(index)
        cached = node_cache[:space][index]
        @index = cached.interval.end if cached
        return cached
      end

      i0, s0 = index, []
      s1, i1 = [], index
      loop do
        if input.index(Regexp.new('[\\s\\n\\r\\t]'), index) == index
          r2 = (SyntaxNode).new(input, index...(index + 1))
          @index += 1
        else
          r2 = nil
        end
        if r2
          s1 << r2
        else
          break
        end
      end
      r1 = SyntaxNode.new(input, i1...index, s1)
      s0 << r1
      if r1
        r4 = _nt_comment
        if r4
          r3 = r4
        else
          r3 = SyntaxNode.new(input, index...index)
        end
        s0 << r3
      end
      if s0.last
        r0 = (SyntaxNode).new(input, i0...index, s0)
        r0.extend(Space0)
      else
        self.index = i0
        r0 = nil
      end

      node_cache[:space][start_index] = r0

      return r0
    end

    module Comment0
    end

    module Comment1
      def space
        elements[2]
      end
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
        r1 = (SyntaxNode).new(input, index...(index + 1))
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
            r5 = (SyntaxNode).new(input, index...(index + 1))
            @index += 1
          else
            r5 = nil
          end
          if r5
            r4 = nil
          else
            self.index = i4
            r4 = SyntaxNode.new(input, index...index)
          end
          s3 << r4
          if r4
            if index < input_length
              r6 = (SyntaxNode).new(input, index...(index + 1))
              @index += 1
            else
              terminal_parse_failure("any character")
              r6 = nil
            end
            s3 << r6
          end
          if s3.last
            r3 = (SyntaxNode).new(input, i3...index, s3)
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
        r2 = SyntaxNode.new(input, i2...index, s2)
        s0 << r2
        if r2
          r7 = _nt_space
          s0 << r7
        end
      end
      if s0.last
        r0 = (SyntaxNode).new(input, i0...index, s0)
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

