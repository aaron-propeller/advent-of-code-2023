sum = 0

File.open('./input.txt').each_line do |line|
  chars = line.strip.split('')
  nums = chars.filter { |c| c.match?(/[[:digit:]]/) }.map(&:to_i)
  sum += "#{nums[0] || 0}#{nums[-1] || 0}".to_i
end

puts sum
