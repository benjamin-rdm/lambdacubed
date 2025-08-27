#!/bin/bash

GRASS="resource_pack/assets/minecraft/textures/block/grass_block_top.png"
WATER="resource_pack/assets/minecraft/textures/block/water_still.png"
LEAVES="resource_pack/assets/minecraft/textures/block/oak_leaves.png"

GRASS_OUT="resource_pack/assets/minecraft/textures/block/grass_block_top_green.png"
WATER_OUT="resource_pack/assets/minecraft/textures/block/water_still_blue.png"
LEAVES_OUT="resource_pack/assets/minecraft/textures/block/oak_leaves_green.png"

convert "$GRASS" -fill "green" -colorize 50% "$GRASS_OUT"

convert "$WATER" -fill "blue" -colorize 50% "$WATER_OUT"

convert "$LEAVES" -fill "#06402B" -colorize 60% "$LEAVES_OUT"

echo "Tinted textures saved as:"
echo "  $GRASS_OUT"
echo "  $WATER_OUT"
echo "  $LEAVES_OUT"
