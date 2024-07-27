#include "api.h"

void api_load_renderer();
void api_load_system();

void api_load()
{
  api_load_system();
  api_load_renderer();
}
