test_that("smoothing only affects windowed area",{
   # Test fixtures
   N <- 101
   p <- 50
   y <- rep(0,N)
   y[p] <- 1

   # Test that only elements along the window are affected
   l <- 10
   l2 <- ceiling(l/2)
   l3 <- p-l2+l+1
   s <- smooth_track(y, window=gausswindow,
                     l=l, normalize=F,
                     symmetric=F, causal=F)

   expect_true(all(s[seq_len(p-l2)] < 1e-10))
   expect_true(all(s[seq(l3,N)]     < 1e-10))

   # Test that only elements along the window are affected
   l <- 11
   l2 <- ceiling(l/2)
   l3 <- p-l2+l+1
   s <- smooth_track(y, window=gausswindow,
                     l=l, normalize=F,
                     symmetric=F, causal=F)

   expect_true(all(s[seq_len(p-l2)] < 1e-10))
   expect_true(all(s[seq(l3,N)]     < 1e-10))

   # Test that only elements along the window are affected
   # NOTE: symmetric=T increases window size
   l <- 10
   l2 <- l-1+1
   s <- smooth_track(y, window=gausswindow,
                     l=l, normalize=F,
                     symmetric=T, causal=F)

   expect_true(all(s[seq_len(p-l2)] < 1e-10))
   expect_true(all(s[seq(p+l2,N)]   < 1e-10))

   # Test that only elements along the window are affected
   # NOTE: symmetric=T increases window size
   l <- 11
   l2 <- l-1+1
   s <- smooth_track(y, window=gausswindow,
                     l=l, normalize=F,
                     symmetric=T, causal=F)

   expect_true(all(s[seq_len(p-l2)] < 1e-10))
   expect_true(all(s[seq(p+l2,N)]   < 1e-10))

   # Test that only elements along the window are affected
   # Causal smoothing should only affect future elements
   l <- 11
   l2 <- ceiling(l/2)
   l3 <- p-l2+l+1
   s <- smooth_track(y, window=gausswindow,
                     l=l, normalize=F,
                     symmetric=F, causal=T)

   expect_true(all(s[seq_len(p-1)] < 1e-10))
   expect_true(all(s[seq(l3,N)]     < 1e-10))
})

test_that("symmetric filtering does not shift phases",{
   # Test fixtures
   N <- 101
   p <- 50
   y <- rep(0,N)
   y[p] <- 1

   wdw <- function(l,...) {
      w <- rep(0,l)
      w[1] <- 1
      w
   }

   s <- smooth_track(y, window=wdw, l=10, normalize=F, symmetric=T, causal=F)

   expect_equal(which(s>0.9),p)
   expect_true(all(s[-p] < 0.001))
})
