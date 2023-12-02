word_to_number_map = {
  "one" => 1,
  "two" => 2,
  "three" => 3,
  "four" => 4,
  "five" => 5,
  "six" => 6,
  "seven" => 7,
  "eight" => 8,
  "nine" => 9,
}
regex = /#{word_to_number_map.keys.join('|')}/

sum = 0

File.open('./input.txt').each_line do |line|
  input_word = line.strip
  duplicated_number_words = word_to_number_map.keys.reduce do |number_word|
    input_word.gsub(number_word, number_word*2)
  end
  numbers_instead_of_words = duplicated_number_words.gsub(regex, word_to_number_map)
  chars = numbers_instead_of_words.split('')
  nums = chars.filter { |c| c.match?(/[[:digit:]]/) }.map(&:to_i)
  sum += "#{nums[0] || 0}#{nums[-1] || 0}".to_i
end

puts sum
