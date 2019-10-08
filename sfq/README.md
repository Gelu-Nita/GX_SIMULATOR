#  Very Fast and Accurate disambiguation code 
This is an IDL code for fast disambiguation of solar vector magnetograms.
The code is also known as SFQ (super fast and quality) and has the following advantages:

 * Works fast (disambiguation of 512x512px magnetogram takes 5-10 seconds on a PC depending on the hardware)
 * Provides disambiguation quality similar to that of Minimum Energy (ME) method.
 * works well with the magnetograms recorded close to the limb
 
## Usage

```sfq_disambig, Bx, By, Bz, pos, Rsun```

 * ```Bx, By, Bz``` - ambiguous magnetic field components
 * ```pos``` - position of the magnetograms ```pos = [x_min, y_min, x_max, y_max]```
 * ```Rsun``` - radius of the Sun given in the same units as ```pos```
 
 [example_hinode.pro](example_hinode.pro) contains an example of processing Hinode SOT/SP magnetogram.

## Acknowledging SFQ
When you publish your result obtained with the use of our code, please city the paper ["Very Fast and Accurate Azimuth Disambiguation of Vector Magnetograms" Rudenko, G. V. And Anfinogentov, S. A.:2014, Solar Physics, Volume 289, Issue 5, pp.1499-15](http://adsabs.harvard.edu/abs/2014SoPh..289.1499R)
