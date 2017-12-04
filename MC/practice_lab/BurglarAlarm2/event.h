#ifndef EVENT_H
#define ENENT_H

// an event to be saved in the EEPROM
typedef struct {
  time_t t;
  byte pin;
  ALARM_TYPE alarm_type;
} EVENT;

#endif /* EVENT_H */


