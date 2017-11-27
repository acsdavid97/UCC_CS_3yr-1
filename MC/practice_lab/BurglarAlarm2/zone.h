#ifndef ZONE_H
#define ZONE_H
#include "Arduino.h"

typedef enum {
  ENTRY_EXIT, DIGITAL, ANALOG, CONTINUOUS
} ALARM_TYPE;

typedef struct {
  ALARM_TYPE alarm_type;
  byte pin;
  int value;
  int state;
  boolean active;
  int time_triggered;
  boolean triggered;
}ZONE;

byte alarm_type2byte(ALARM_TYPE at);

ALARM_TYPE byte2alarm_type(byte b);

#endif /* ZONE_H */
