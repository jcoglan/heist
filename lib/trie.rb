module Heist
  class Trie
    attr_accessor :value
    
    def initialize(value = nil)
      @value = value
      @children = {}
    end
    
    def has_key?(key)
      trie = traverse(key)
      trie and not trie.value.nil?
    end
    
    def [](key)
      trie = traverse(key)
      trie ? trie.value : nil
    end
    
    def []=(key, value)
      traverse(key, true).value = value
    end
    
    def traverse(key, create_if_absent = false)
      key = convert_key(key)
      return self if key.empty?
      trie = @children[key.first]
      return nil if trie.nil? and not create_if_absent
      trie = @children[key.first] = Trie.new if trie.nil?
      trie.traverse(key[1..-1], create_if_absent)
    end
    
    def prefixes
      @children.keys
    end
    
    def singular?
      @value.nil? and @children.size == 1
    end
    
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
    
    def inspect
      @children.inspect
    end
    
  private
    
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

