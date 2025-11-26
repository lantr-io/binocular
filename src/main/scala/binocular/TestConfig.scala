package binocular

/** Test configuration for the validator.
  *
  * Set TestMode = true only for testing with simulated time. In production, this must be false to
  * ensure time tolerance checks are enforced.
  */
object TestConfig {
    inline def TestMode: Boolean = false
}
