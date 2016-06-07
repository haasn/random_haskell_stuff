instance Num n => Num (Maybe n) where
  (Just a) + (Just b) = Just (a+b)
  _ + _ = Nothing

  (Just a) * (Just b) = Just (a*b)
  _ * _ = Nothing

  abs = fmap abs
  signum = fmap signum
  fromInteger = Just . fromInteger

instance (Eq n, Fractional n) => Fractional (Maybe n) where
  fromRational = Just . fromRational

  -- Special division with failure
  _ / 0 = Nothing
  (Just a) / (Just b) = Just (a/b)
  _ / _ = Nothing
