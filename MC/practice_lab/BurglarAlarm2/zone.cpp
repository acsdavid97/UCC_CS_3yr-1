#include "zone.h"

//convert alarm type to byte
byte alarm_type2byte(ALARM_TYPE at) {
  switch (at) {
    case ENTRY_EXIT:
      return 0;
    case DIGITAL:
      return 1;
    case ANALOG:
      return 2;
    case CONTINUOUS:
      return 3;
  }
  return 0;
}

// converts a byte to alarm type
ALARM_TYPE byte2alarm_type(byte b) {
  switch (b) {
    case 0:
      return ENTRY_EXIT;
    case 1:
      return DIGITAL;
    case 2:
      return ANALOG;
    case 3:
      return CONTINUOUS;
  }
  return ENTRY_EXIT;
}
