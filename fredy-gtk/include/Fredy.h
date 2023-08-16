#ifndef Fredy_h
#define Fredy_h

#include <gtkmm/window.h>
#include <gtkmm/button.h>
#include <gtkmm/grid.h>
#include <gtkmm/label.h>

namespace fredy
{
    /**
     *
     */
    class FredyWindow : public Gtk::Window
    {
    public:
        /**
         * Creates hello world window
         */
        FredyWindow();

        /**
         * Delete hello world window
         */
        virtual ~FredyWindow();

    protected:
        /**
         * Signal handlers
         */
        void on_button_clicked();

        int counter;

        // Member widgets:
        Gtk::Grid m_grid;
        Gtk::Button m_button;
        Gtk::Label m_label;
    };
}

#endif
