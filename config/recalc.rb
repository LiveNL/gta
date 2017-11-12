require 'multi_json'

#file = File.open('config/realworld.json')
file = File.open('world.ai.txt')
json = MultiJson.load file, symbolize_keys: true

all = json[:children]
grouped = all.group_by {|x| x[:name]}

rds = grouped["Road"].first[:children]
bds = grouped["Building"].first[:children]
sws = grouped["Sidewalk"].first[:children]
wls = grouped["Wall"].first[:children]
trs = grouped["Tree"].first[:children]
cns = grouped["Coin"].first[:children]
newJson = {}

rds.map{|x| x[:name] = "Road"}
bds.map{|x| x[:name] = "Building"}
sws.map{|x| x[:name] = "Sidewalk"}
wls.map{|x| x[:name] = "Wall"}
trs.map{|x| x[:name] = "Tree"}
cns.map{|x| x[:name] = "Coin"}

blocks = sws + rds + bds + wls + trs + cns

blocks_json = blocks.map do |block|
  x = block[:x].to_f + (block[:w].to_f / 2)
  y = block[:y].to_f + (block[:h].to_f / 2)

  types = block[:name] == "Tree" ? ["tree1","tree2"] : [block[:name]]

  {"blockPosition": { "x": x, "y": y}, "blockWidth": block[:w].to_f, "blockHeight": block[:h].to_f, "blockType": block[:name],
   "blockSprite": { "spriteType": types.sample.downcase, "spriteState": 1 }}
end

cars_json = grouped["Car"].first[:children].map do |car|
  x = car[:x].to_f + (car[:w].to_f / 2)
  y = car[:y].to_f + (car[:h].to_f / 2)

  dir = car[:name] == "None" ? "North" : car[:name]
  v = car[:name] == "None" ? 0 : 1

  types = ["car1","car2","car3", "car4", "car5", "car6", "car7", "car8", "car9", "car10", "car11", "car12"]

  {"carPosition": { "x": x, "y": y}, "carSprite": { "spriteType": types.sample, "spriteState": 1 },
   "carDirection": dir, "velocity": v}

end

people_json = grouped["Person"].first[:children].map do |person|
  x = person[:x].to_f + (person[:w].to_f / 2)
  y = person[:y].to_f + (person[:h].to_f / 2)

  types = ["person1", "person2"]
  {"personPosition": { "x": x, "y": y}, "personSprite": { "spriteType": types.sample, "spriteState": 1 }, "personDirection": "North", "personVelocity": 1 }
end

newJson[:blocksJSON] = blocks_json
newJson[:peopleJSON] = people_json
newJson[:carsJSON] = cars_json
newJson[:highscoreJSON] = 0

n = MultiJson.dump newJson, pretty: true
File.open("config/world.json", 'w') { |file| file.write(n) }
