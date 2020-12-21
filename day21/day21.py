from sys import stdin

def pluck(s): return next(iter(s))

def find_allergen_containers(food_items):   
    all_allergens = {allergen for _, allergens in food_items for allergen in allergens}
    allergen_containers = {}
    while True:
        fixpoint = True
        for allergen in all_allergens:
            if allergen in allergen_containers:
                continue
            possible_ingredients = set()
            for ingredients, allergens in food_items:
                if allergen in allergens:
                    possible_ingredients |= set(ingredients)
            possible_ingredients -= set(allergen_containers.values())
            for ingredients, allergens in food_items:
                if allergen in allergens:
                    possible_ingredients &= set(ingredients)
            if len(possible_ingredients) == 1:
                allergen_containers[allergen] = pluck(possible_ingredients)
                fixpoint = False
        if fixpoint:
            break
    return allergen_containers

def count_allergenless_ingredients(food_items):
    allergen_containers = find_allergen_containers(food_items)
    all_ingredients = {ingredient for ingredients, _ in food_items for ingredient in ingredients}
    allergenless_ingredients = all_ingredients - set(allergen_containers.values())
    all_ingredients_list = [ingredient for ingredients, _ in food_items for ingredient in ingredients]
    return sum(ingredient in allergenless_ingredients for ingredient in all_ingredients_list)

def list_dangerous_ingredients(food_items):
    allergen_containers = find_allergen_containers(food_items)
    return [x[1] for x in sorted(allergen_containers.items(), key=lambda x: x[0])]

food_items = []
for line in stdin:
    first_half, second_half = line.strip().split(' (contains ')
    ingredients = first_half.split()
    allergens = second_half[:-1].split(', ')
    food_items.append((ingredients, allergens))

print(count_allergenless_ingredients(food_items))
print(','.join(list_dangerous_ingredients(food_items)))
