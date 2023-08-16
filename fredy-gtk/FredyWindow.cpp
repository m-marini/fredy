#include <stdio.h>
#include <iostream>

#include <gtkmm/application.h>

#include "Fredy.h"

using namespace fredy;

int main(int argc, char *argv[])
{
    auto app = Gtk::Application::create(argc, argv, "org.mmarini.fredy");
    FredyWindow windows;

    // Shows the window and returns when it is closed.
    return app->run(windows);
}


FredyWindow::FredyWindow()
    : m_button("Increment"),
      m_label("Counter 0")
{
  set_title("Gtk::Grid");
  set_border_width(10);

  // m_grid.set_margin(12);
  add(m_grid);

  // Sets the border width of the window.

  // When the button receives the "clicked" signal, it will call the
  // on_button_clicked() method defined below.
  m_button.signal_clicked().connect(sigc::mem_fun(*this,
                                                  &FredyWindow::on_button_clicked));
  m_grid.attach(m_label, 0, 0);
  m_grid.attach(m_button, 0, 1);

  // The final step is to display this newly created widget...
  m_grid.show();
  m_label.show();
  m_button.show();
}

FredyWindow::~FredyWindow()
{
}

void FredyWindow::on_button_clicked()
{
  counter++;
  char msg[100];
  sprintf(msg, "Counter %d", counter);
  m_label.set_text(msg);
}
