test_that("smoothing returns the correct length",{
   N <- 101
   y <- seq_len(N)

   # Test for odd and even length without causality and
   # forward-backward filtering
   expect_length(smooth_track(y, window=gausswindow, l=10, normalize=F,
                              symmetric=F, causal=F), N)

   expect_length(smooth_track(y, window=gausswindow, l=11, normalize=F,
                              symmetric=F, causal=F), N)

   # Test for odd and even length without causality and
   # with forward-backward filtering
   expect_length(smooth_track(y, window=gausswindow, l=10, normalize=F,
                              symmetric=T, causal=F), N)

   expect_length(smooth_track(y, window=gausswindow, l=11, normalize=F,
                              symmetric=T, causal=F), N)

   # Test for odd and even length with causality and
   # without forward-backward filtering
   expect_length(smooth_track(y, window=gausswindow, l=10, normalize=F,
                              symmetric=F, causal=T), N)

   expect_length(smooth_track(y, window=gausswindow, l=11, normalize=F,
                              symmetric=F, causal=T), N)

})
