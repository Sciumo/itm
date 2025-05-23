/** @file FreeSpaceLoss.cpp
 * Implements a simple function to calculate free space basic transmission loss
 */
#include "ITS.Propagation.ITM/ITM.h"

namespace ITS {
namespace Propagation {
namespace ITM {

/*=============================================================================
 |
 |  Description:  Free space basic transmission loss equation
 |
 |        Input:  d__meter       - Path distance, in meters
 |                f__mhz         - Frequency, in MHz
 |
 |      Outputs:  [None]
 |
 |      Returns:  A_fs__db       - Free space basic transmission loss, in dB
 |
 *===========================================================================*/
double FreeSpaceLoss(double d__meter, double f__mhz)
{
    return 32.45 + 20.0 * log10(f__mhz) + 20.0 * log10(d__meter / 1000.0);
}

}  // namespace ITM
}  // namespace Propagation
}  // namespace ITS