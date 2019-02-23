#ifdef USE_JOYSTICK_MOUSE
class JoyMouse {
	private:
		int xpos;
		int ypos;
		int pushed;
		int addr;
		int xcenter;
		int ycenter;
		int width;
		int height;

	public:
		JoyMouse(int _addr);
		void setup(int w, int h, int _xc, int _yc);
		void hide();
		void show();
		void poll();
		int x();
		int y();
		int button();
};
extern JoyMouse mouse;
#endif /* USE_JOYSTICK_MOUSE */
