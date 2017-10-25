## smash, squish, smack, squash, snort, sniff, screech, splash,
## squeeze, smush, swat, splat, spill, swat, shred, splice, slit,
## spear, slap, strike, smooth, snap, squat, squeek, squeeze,
## squiggle, squirm, slump, slug, slosh, slow, stamp

library(tfse)
make_package()

rt <- rtweet::search_tweets("lang:en", n = 200)

options(width = 180)
squish(rt)
