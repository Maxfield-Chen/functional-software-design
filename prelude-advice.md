http://dev.stephendiehl.com/hask/#what-to-avoid

# Avoid String.
# Use fmap instead of map.
# Use Foldable and Traversable instead of the Control.Monad, and Data.List versions of traversals.
# Avoid partial functions like head and read or use their total variants.
# Avoid exceptions, use ExceptT or Either instead.
# Avoid boolean blind functions.

