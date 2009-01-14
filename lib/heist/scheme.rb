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
        i1 = index
        r2 = _nt_comment
        if r2
          r1 = r2
        else
          r3 = _nt_cell
          if r3
            r1 = r3
          else
            self.index = i1
            r1 = nil
          end
        end
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

    module Comment0
    end

    module Comment1
      def space
        elements[0]
      end

      def space
        elements[3]
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
      r1 = _nt_space
      s0 << r1
      if r1
        if input.index(";", index) == index
          r2 = (SyntaxNode).new(input, index...(index + 1))
          @index += 1
        else
          terminal_parse_failure(";")
          r2 = nil
        end
        s0 << r2
        if r2
          s3, i3 = [], index
          loop do
            i4, s4 = index, []
            i5 = index
            if input.index(Regexp.new('[\\n\\r]'), index) == index
              r6 = (SyntaxNode).new(input, index...(index + 1))
              @index += 1
            else
              r6 = nil
            end
            if r6
              r5 = nil
            else
              self.index = i5
              r5 = SyntaxNode.new(input, index...index)
            end
            s4 << r5
            if r5
              if index < input_length
                r7 = (SyntaxNode).new(input, index...(index + 1))
                @index += 1
              else
                terminal_parse_failure("any character")
                r7 = nil
              end
              s4 << r7
            end
            if s4.last
              r4 = (SyntaxNode).new(input, i4...index, s4)
              r4.extend(Comment0)
            else
              self.index = i4
              r4 = nil
            end
            if r4
              s3 << r4
            else
              break
            end
          end
          r3 = SyntaxNode.new(input, i3...index, s3)
          s0 << r3
          if r3
            r8 = _nt_space
            s0 << r8
          end
        end
      end
      if s0.last
        r0 = (Comment).new(input, i0...index, s0)
        r0.extend(Comment1)
      else
        self.index = i0
        r0 = nil
      end

      node_cache[:comment][start_index] = r0

      return r0
    end

    module Cell0
      def space
        elements[0]
      end

      def space
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

      i0, s0 = index, []
      r1 = _nt_space
      s0 << r1
      if r1
        i2 = index
        r3 = _nt_list
        if r3
          r2 = r3
        else
          r4 = _nt_atom
          if r4
            r2 = r4
          else
            self.index = i2
            r2 = nil
          end
        end
        s0 << r2
        if r2
          r5 = _nt_space
          s0 << r5
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

    module List0
    end

    def _nt_list
      start_index = index
      if node_cache[:list].has_key?(index)
        cached = node_cache[:list][index]
        @index = cached.interval.end if cached
        return cached
      end

      i0, s0 = index, []
      if input.index("(", index) == index
        r1 = (SyntaxNode).new(input, index...(index + 1))
        @index += 1
      else
        terminal_parse_failure("(")
        r1 = nil
      end
      s0 << r1
      if r1
        s2, i2 = [], index
        loop do
          r3 = _nt_cell
          if r3
            s2 << r3
          else
            break
          end
        end
        r2 = SyntaxNode.new(input, i2...index, s2)
        s0 << r2
        if r2
          if input.index(")", index) == index
            r4 = (SyntaxNode).new(input, index...(index + 1))
            @index += 1
          else
            terminal_parse_failure(")")
            r4 = nil
          end
          s0 << r4
        end
      end
      if s0.last
        r0 = (List).new(input, i0...index, s0)
        r0.extend(List0)
      else
        self.index = i0
        r0 = nil
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
      r1 = _nt_number
      if r1
        r0 = r1
        r0.extend(Atom)
      else
        r2 = _nt_string
        if r2
          r0 = r2
          r0.extend(Atom)
        else
          r3 = _nt_symbol
          if r3
            r0 = r3
            r0.extend(Atom)
          else
            self.index = i0
            r0 = nil
          end
        end
      end

      node_cache[:atom][start_index] = r0

      return r0
    end

    module Number0
    end

    module Number1
    end

    module Number2
    end

    def _nt_number
      start_index = index
      if node_cache[:number].has_key?(index)
        cached = node_cache[:number][index]
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
            r5.extend(Number0)
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
        if r3
          i10, s10 = index, []
          if input.index(".", index) == index
            r11 = (SyntaxNode).new(input, index...(index + 1))
            @index += 1
          else
            terminal_parse_failure(".")
            r11 = nil
          end
          s10 << r11
          if r11
            s12, i12 = [], index
            loop do
              r13 = _nt_digit
              if r13
                s12 << r13
              else
                break
              end
            end
            if s12.empty?
              self.index = i12
              r12 = nil
            else
              r12 = SyntaxNode.new(input, i12...index, s12)
            end
            s10 << r12
          end
          if s10.last
            r10 = (SyntaxNode).new(input, i10...index, s10)
            r10.extend(Number1)
          else
            self.index = i10
            r10 = nil
          end
          if r10
            r9 = r10
          else
            r9 = SyntaxNode.new(input, index...index)
          end
          s0 << r9
        end
      end
      if s0.last
        r0 = (Number).new(input, i0...index, s0)
        r0.extend(Number2)
      else
        self.index = i0
        r0 = nil
      end

      node_cache[:number][start_index] = r0

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

    def _nt_symbol
      start_index = index
      if node_cache[:symbol].has_key?(index)
        cached = node_cache[:symbol][index]
        @index = cached.interval.end if cached
        return cached
      end

      s0, i0 = [], index
      loop do
        if input.index(Regexp.new('[^\\(\\)\\s]'), index) == index
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
        r0 = Symbol.new(input, i0...index, s0)
      end

      node_cache[:symbol][start_index] = r0

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

    def _nt_space
      start_index = index
      if node_cache[:space].has_key?(index)
        cached = node_cache[:space][index]
        @index = cached.interval.end if cached
        return cached
      end

      s0, i0 = [], index
      loop do
        if input.index(Regexp.new('[\\s\\n\\r\\t]'), index) == index
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
      r0 = SyntaxNode.new(input, i0...index, s0)

      node_cache[:space][start_index] = r0

      return r0
    end

  end

  class SchemeParser < Treetop::Runtime::CompiledParser
    include Scheme
  end

end

