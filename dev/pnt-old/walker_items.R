# walker =
#   tibble(
#     walker = c(rep("A",30), rep("B", 30)),
#     walker_order = rep(seq(1,30,1),2),
#     target = c(
#         'wagon',
#         'monkey',
#         'spoon',
#         'ring',
#         'hammer',
#         'crown',
#         'ghost',
#         'turkey',
#         'hat',
#         'pumpkin',
#         'baby',
#         'scissors',
#         'tent',
#         'squirrel',
#         'foot',
#         'candle',
#         'leaf',
#         'pillow',
#         'bread',
#         'owl',
#         'hair',
#         'clown',
#         'hose',
#         'kitchen',
#         'strawberries',
#         'calendar',
#         'bus',
#         'sock',
#         'dice',
#         'basket',
#     ################
#         'thermometer',
#         'piano',
#         'queen',
#         'butterfly',
#         'sandwich',
#         'bone',
#         'king',
#         'vest',
#         'skull',
#         'horse',
#         'rake',
#         'drum',
#         'table',
#         'pig',
#         'camera',
#         'flower',
#         'cane',
#         'house',
#         'duck',
#         'apple',
#         'skis',
#         'door',
#         'carrot',
#         'whistle',
#         'tractor',
#         'glove',
#         'desk',
#         'saw',
#         'anchor',
#         'pencil'
#     )
#   )
# 
# #check.
# sum(walker$target %in% item_key$target)
# 
# 
# 
# item_key <- item_key %>% left_join(walker, by = "target")
# items <- items %>% left_join(walker, by = "target")
