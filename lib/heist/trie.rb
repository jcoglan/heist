module Heist
  
  # A +Trie+ is a special type of key-value store, one in which the keys are all
  # sequences of some kind: they could be strings, or arrays, or lists. We use
  # tries to store symbol tables in Heist because it makes autocompletion easier
  # to implement, and one aim of Heist is to be a pleasant interactive Scheme.
  #
  # In our particular implementation, keys (strings representing variable names)
  # are split into arrays and stored in a series of nested hashtables whose keys
  # are single characters. Strings sharing a common prefix share part of the tree
  # structure. The <tt>Trie#inspect</tt> method provides a good representation
  # of this (values omitted for the sake of brevity):
  #
  #   tree = Trie.new
  #   tree['foo'] = 1
  #   
  #   tree #=> {"f"=>{"o"=>{"o"=>{}}}}
  #   
  #   tree['bar'] = 2
  #   tree['baz'] = 3
  #   
  #   tree #=> {"b"=>{"a"=>{"z"=>{}, "r"=>{}}}, "f"=>{"o"=>{"o"=>{}}}}
  #
  # A trie is a recursive structure; each key in a trie points to another trie.
  # Every trie contains a list of its children (a hash mapping characters to
  # subtries) and may contain a value if it appears at the end of a sequence
  # making up a full key of the root trie.
  #
  # For more information, see http://en.wikipedia.org/wiki/Trie
  #
  class Trie
    attr_accessor :value
    
    # A +Trie+ is initialized with an optional +value+. The value may be
    # omitted if the trie does not represent the end of a key sequence.
    def initialize(value = nil)
      @value = value
      @children = {}
    end
    
    # Iterates over each child key of the +Trie+, calling the given +block+
    # with each key and corresponding value. Like iterating over a flat +Hash+.
    def each_child
      @children.each { |key, subtree| yield(key, subtree) }
    end
    
    # Iterates over the whole +Trie+, calling the given +block+ with each
    # composite key and corresponding value. Each key will be an +Array+ matching
    # the route to get from the root of the +Trie+ to the current node.
    def each(prefix = [], &block)
      each_child { |path, subtree| subtree.each(prefix + [path], &block) }
      yield(prefix, @value) unless @value.nil?
    end
    
    # Returns +true+ iff the given +key+ is present in the +Trie+. They key
    # must match a complete key sequence that maps to a value, not a partial
    # prefix with no value attached.
    def has_key?(key)
      trie = traverse(key)
      trie and not trie.value.nil?
    end
    
    # Returns the value corresponding to +key+, or +nil+ if none is found.
    def [](key)
      trie = traverse(key)
      trie ? trie.value : nil
    end
    
    # Assigns +value+ to the given +key+.
    def []=(key, value)
      traverse(key, true).value = value
    end
    
    # Walks the +Trie+ structure using the given +key+, returning the
    # resulting subtrie or +nil+ if the key is absent. Pass +true+ as the
    # second argument to create a subtrie for the key if none exists.
    def traverse(key, create_if_absent = false)
      key = convert_key(key)
      return self if key.empty?
      trie = @children[key.first]
      return nil if trie.nil? and not create_if_absent
      trie = @children[key.first] = Trie.new if trie.nil?
      trie.traverse(key[1..-1], create_if_absent)
    end
    
    # Returns an array of all the characters used as keys in this +Trie+.
    # That is, this method returns the initial letters of all the keys
    # stored in the +Trie+.
    def prefixes
      @children.keys
    end
    
    # Returns +true+ if the +Trie+ has no value and only one child. Used
    # in +longest_prefix+ to find the longest unique prefix from some
    # starting point.
    def singular?
      @value.nil? and @children.size == 1
    end
    
    # Returns the longest unique key prefix that starts with +key+. This
    # is used for autocompletion in the REPL. Given a string, this method
    # returns another string such that the start of the output is the
    # same as the input, and the output may contain zero or more additional
    # characters such that all the keys that begin with the input also begin
    # with the output.
    #
    #   tree = Trie.new
    #   tree['foo'] = 1
    #   tree['bar'] = 2
    #   tree['baz'] = 3
    #   
    #   tree.longest_prefix 'f'
    #   #=> "foo"
    #   tree.longest_prefix 'b'
    #   #=> "ba"
    #
    def longest_prefix(key)
      prefix = convert_key(key).dup
      trie = traverse(key)
      return nil if trie.nil?
      while trie.singular?
        next_key = trie.prefixes.first
        prefix << next_key
        trie = trie.traverse(next_key)
      end
      case key
        when String then prefix.join('')
        when Symbol then prefix.join('').to_sym
        else prefix
      end
    end
    
    # Returns a string representation of the trie's internal structure.
    # Values are not printed; the main purpose of this method is to inspect
    # the internal tree structure.
    def inspect
      @children.inspect
    end
    
  private
    
    # Returns an array from the given +key+, making it ready for use as
    # a key sequence.
    def convert_key(key)
      case key
      when Array      then key
      when Symbol     then key.to_s.split('')
      when String     then key.split('')
      when Enumerable then key.entries
      end
    end
  end
  
end

