/*
*********************************************************************************************************
*                                 STM32F4 VIRTUAL FRAME BUFFER DRIVER	
*
*                          (c) Copyright 2013; robutest, Inc.; Istanbul, TR
*
*  				This file is subject to the terms and conditions of the GNU General Public
*  				License. See the file COPYING in the main directory of this archive for
*  				more details.
*********************************************************************************************************
*/

/*
*********************************************************************************************************
*
*                                 STM32F4 VIRTUAL FRAME BUFFER DRIVER
*
*                                       	  STM32F429ZI
*
* Filename      : stm32fb.c
* Version       : V1.00
* Programmer(s) : tmk
*********************************************************************************************************
*/

/*
*********************************************************************************************************
*                                            INCLUDE FILES
*********************************************************************************************************
*/

#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/errno.h>
#include <linux/string.h>
#include <linux/mm.h>
#include <linux/slab.h>
#include <linux/delay.h>
#include <linux/interrupt.h>
#include <linux/platform_device.h>
#include <linux/fb.h>
#include <linux/init.h>
#include <linux/module.h>
#include <linux/uaccess.h>

#if 1
#define DEBUG(...)
#else
#define DEBUG   printk
#endif



/*
*********************************************************************************************************
*                                           LOCAL DEFINITIONS
*********************************************************************************************************
*/

/*
 *  RAM we reserve for the frame buffer. This defines the maximum screen
 *  size
 *
 *  The default can be overridden if the driver is compiled as a module
 */

#ifdef  CONFIG_ARCH_STM32
#define STM32FB_BASE_PHYS   (0x90700000)
#define STM32FB_FB_LEN      (1 * 1024 * 1024)
#define STM32FB_FB_PHYS     (STM32FB_BASE_PHYS)
#else
#error  "This driver must be used in STM32!"
#endif


/*
*********************************************************************************************************
*                                           LOCAL VARIABLES
*********************************************************************************************************
*/

static u_long vfb_size;
static u_long vfb_addr;
static bool   vfb_enab;

module_param(vfb_enab, bool,  0);
module_param(vfb_size, ulong, 0);
module_param(vfb_addr, ulong, 0);


static struct fb_var_screeninfo vfb_default __initdata = {
	.xres 			= 240,
	.yres 			= 320,
	.xres_virtual   = 240,
	.yres_virtual   = 320,
	.xoffset		= 0,
	.yoffset		= 0,
	.bits_per_pixel = 16,
    .grayscale		= 0,
	.red 			= { 11, 5, 0 },
  	.green 			= {  5, 6, 0 },
    .blue 			= {  0, 5, 0 },
    .transp 		= {  0, 0, 0 },
    .activate 		= FB_ACTIVATE_NOW,
    .height 		= -1,
    .width 			= -1,
   	.vmode 			= FB_VMODE_NONINTERLACED,
   	.rotate			= 0,
};

static struct fb_fix_screeninfo vfb_fix __initdata = {
	.id 		 = "STM32 VFB",
	.smem_start	 = STM32FB_FB_PHYS,
	.smem_len	 = STM32FB_FB_LEN,
	.type 		 = FB_TYPE_PACKED_PIXELS,
	.visual 	 = FB_VISUAL_TRUECOLOR,
	.xpanstep 	 = 0,
	.ypanstep 	 = 0,
	.ywrapstep   = 0,
	.line_length = 480,
	.accel 		 = FB_ACCEL_NONE,
};



/*
*********************************************************************************************************
*                                         LOCAL FUNCTION PROTOTYPES
*********************************************************************************************************
*/

static int vfb_check_var(struct fb_var_screeninfo *var,
			 struct fb_info *info);
static int vfb_set_par(struct fb_info *info);
static int vfb_setcolreg(u_int regno, u_int red, u_int green, u_int blue,
			 u_int transp, struct fb_info *info);
static int vfb_pan_display(struct fb_var_screeninfo *var,
			   struct fb_info *info);
static ssize_t vfb_sys_read(struct fb_info *info, char __user *buf, size_t count,
		    loff_t *ppos);
static ssize_t vfb_sys_write(struct fb_info *info, const char __user *buf,
		     size_t count, loff_t *ppos);


static struct fb_ops vfb_ops = {
	.owner 			= THIS_MODULE,
	//.fb_read        = vfb_sys_read,
	//.fb_write       = vfb_sys_write,
	.fb_check_var	= vfb_check_var,
	.fb_set_par		= vfb_set_par,
	.fb_setcolreg	= vfb_setcolreg,
	.fb_pan_display	= vfb_pan_display,
    .fb_fillrect    = cfb_fillrect,
    .fb_copyarea    = cfb_copyarea,
    .fb_imageblit   = cfb_imageblit,
};

/*
*********************************************************************************************************
*                                         	LOCAL FUNCTIONS
*********************************************************************************************************
*/

static inline u_int chan_to_field(u_int chan, struct fb_bitfield *bf)
{
	chan &= 0xffff;
	chan >>= (16 - bf->length);

	return (chan << bf->offset);
}//chan_to_field


/*******************************************************************************
* Function Name  : copy_from_user16
* Description    : None
* Input          : None
* Output         : None
* Return         : None
*******************************************************************************/
static inline unsigned long copy_from_user16(void *to, const void *from,
                         unsigned long n)
{
    u16 *dst = (u16 *) to;
    u16 *src = (u16 *) from;

    while (n > 1) 
    {
        u16 v;
        if (__get_user(v, src))
            return n;

        fb_writew(v, dst);

        src++, dst++;
        n -= 2;
    }//while

    if (n) 
    {
        u8 v;

        if (__get_user(v, ((u8 *) src)))
            return n;

        fb_writeb(v, dst);
    }//if

    return 0;
}//copy_from_user16


/*******************************************************************************
* Function Name  : copy_to_user16
* Description    : None
* Input          : None
* Output         : None
* Return         : None
*******************************************************************************/
static inline unsigned long copy_to_user16(void *to, const void *from,
                       unsigned long n)
{
    u16 *dst = (u16 *) to;
    u16 *src = (u16 *) from;

    while (n > 1) 
    {
        u16 v = fb_readw(src);

        if (__put_user(v, dst))
            return n;

        src++, dst++;
        n -= 2;
    }//while

    if (n) 
    {
        u8 v = fb_readb(src);

        if (__put_user(v, ((u8 *) dst)))
            return n;
    }//if

    return 0;
}//copy_to_user16


/*******************************************************************************
* Function Name  : vfb_sys_read
* Description    : None
* Input          : None
* Output         : None
* Return         : None
*******************************************************************************/
static ssize_t vfb_sys_read(struct fb_info *info, char __user *buf, size_t count,
		    loff_t *ppos)
{
    unsigned long p = *ppos;

    if (p >= info->fix.smem_len)
        return 0;
    if (count >= info->fix.smem_len)
        count = info->fix.smem_len;
    if (count + p > info->fix.smem_len)
        count = info->fix.smem_len - p;

    if (count) 
    {
        char *base_addr;

        base_addr = info->screen_base;
        count -= copy_to_user16(buf, base_addr + p, count);
        if (!count)
            return -EFAULT;
        *ppos += count;
    }//if

    return count;
}//vfb_sys_read


/*******************************************************************************
* Function Name  : vfb_sys_write
* Description    : None
* Input          : None
* Output         : None
* Return         : None
*******************************************************************************/
static ssize_t vfb_sys_write(struct fb_info *info, const char __user *buf,
		     size_t count, loff_t *ppos)
{
    unsigned long p = *ppos;
    int err = 0;

    /* from fbmem.c except for our own copy_*_user */
    if (p > info->fix.smem_len)
        return -ENOSPC;

    if (count >= info->fix.smem_len)
        count = info->fix.smem_len;

    if (count + p > info->fix.smem_len) 
    {
        count = info->fix.smem_len - p;
        err = -ENOSPC;
    }//if

    if (count) 
    {
        char *base_addr;

        base_addr = info->screen_base;
        count -= copy_from_user16(base_addr + p, buf, count);
        *ppos += count;
        err = -EFAULT;
    }//if

    if (count)
        return count;
    
    return err;
}//vfb_sys_write


/*******************************************************************************
* Function Name  : get_line_length
* Description    : None
* Input          : None
* Output         : None
* Return         : None
*******************************************************************************/
static u_long get_line_length(int xres_virtual, int bpp)
{
	u_long length;

	length = xres_virtual * bpp;
	length = (length + 31) & ~31;
	length >>= 3;

	return (length);
}//get_line_length

/*
 *  Setting the video mode has been split into two parts.
 *  First part, xxxfb_check_var, must not write anything
 *  to hardware, it should only verify and adjust var.
 *  This means it doesn't alter par but it does use hardware
 *  data from it to check this var. 
 */
static int vfb_check_var(struct fb_var_screeninfo *var,
			 struct fb_info *info)
{
	u_long line_length;

	/*
	 *  FB_VMODE_CONUPDATE and FB_VMODE_SMOOTH_XPAN are equal!
	 *  as FB_VMODE_SMOOTH_XPAN is only used internally
	 */

	if (var->vmode & FB_VMODE_CONUPDATE) 
	{
		var->vmode |= FB_VMODE_YWRAP;
		var->xoffset = info->var.xoffset;
		var->yoffset = info->var.yoffset;
	}//if

	/*
	 *  Some very basic checks
	 */
	if (!var->xres)
		var->xres = 1;
	if (!var->yres)
		var->yres = 1;
	if (var->xres > var->xres_virtual)
		var->xres_virtual = var->xres;
	if (var->yres > var->yres_virtual)
		var->yres_virtual = var->yres;
	if (var->bits_per_pixel <= 1)
		var->bits_per_pixel = 1;
	else if (var->bits_per_pixel <= 8)
		var->bits_per_pixel = 8;
	else if (var->bits_per_pixel <= 16)
		var->bits_per_pixel = 16;
	else if (var->bits_per_pixel <= 24)
		var->bits_per_pixel = 24;
	else if (var->bits_per_pixel <= 32)
		var->bits_per_pixel = 32;
	else
		return -EINVAL;

	if (var->xres_virtual < var->xoffset + var->xres)
		var->xres_virtual = var->xoffset + var->xres;
	if (var->yres_virtual < var->yoffset + var->yres)
		var->yres_virtual = var->yoffset + var->yres;

	/*
	 *  Memory limit
	 */
	line_length =
	    get_line_length(var->xres_virtual, var->bits_per_pixel);
	if (line_length * var->yres_virtual > info->screen_size)
		return -ENOMEM;

	/*
	 * Now that we checked it we alter var. The reason being is that the video
	 * mode passed in might not work but slight changes to it might make it 
	 * work. This way we let the user know what is acceptable.
	 */
	switch (var->bits_per_pixel) {
	case 1:
	case 8:
		var->red.offset    = 0;
		var->red.length    = 8;
		var->green.offset  = 0;
		var->green.length  = 8;
		var->blue.offset   = 0;
		var->blue.length   = 8;
		var->transp.offset = 0;
		var->transp.length = 0;
		break;

	case 16:		
		/* RGBA 5551 */
		if (var->transp.length) 
		{
			var->red.offset    = 11;
			var->red.length    = 5;
			var->green.offset  = 6;
			var->green.length  = 5;
			var->blue.offset   = 1;
			var->blue.length   = 5;
			var->transp.offset = 0;
			var->transp.length = 1;
		}//if
		else 
		{	/* RGB 565 */
			var->red.offset    = 11;
			var->red.length    = 5;
			var->green.offset  = 5;
			var->green.length  = 6;
			var->blue.offset   = 0;
			var->blue.length   = 5;
			var->transp.offset = 0;
			var->transp.length = 0;
		}//else
		break;

	case 24:		
		/* RGB 888 */
		var->red.offset    = 0;
		var->red.length    = 8;
		var->green.offset  = 8;
		var->green.length  = 8;
		var->blue.offset   = 16;
		var->blue.length   = 8;
		var->transp.offset = 0;
		var->transp.length = 0;
		break;

	case 32:		
		/* RGBA 8888 */
		var->red.offset    = 24;
		var->red.length    = 8;
		var->green.offset  = 16;
		var->green.length  = 8;
		var->blue.offset   = 8;
		var->blue.length   = 8;
		var->transp.offset = 0;
		var->transp.length = 8;
		break;
	}//switch
	var->red.msb_right    = 0;
	var->green.msb_right  = 0;
	var->blue.msb_right   = 0;
	var->transp.msb_right = 0;

	return 0;
}//vfb_check_var


/* This routine actually sets the video mode. It's in here where we
 * the hardware state info->par and fix which can be affected by the 
 * change in par. For this driver it doesn't do much. 
 */
static int vfb_set_par(struct fb_info *info)
{
	info->fix.line_length = get_line_length(info->var.xres_virtual,
						info->var.bits_per_pixel);
	return 0;
}//vfb_set_par

/*
 *  Set a single color register. The values supplied are already
 *  rounded down to the hardware's capabilities (according to the
 *  entries in the var structure). Return != 0 for invalid regno.
 */
static int vfb_setcolreg(u_int regno, u_int red, u_int green, u_int blue,
			 u_int transp, struct fb_info *info)
{
	unsigned int val;
	int ret = 1;

	/*
	 * If greyscale is true, then we convert the RGB value
	 * to greyscale no mater what visual we are using.
	 */
	if (info->var.grayscale) 
		red = green = blue = (19595 * red + 38470 * green +
					7471 * blue) >> 16;

	switch (info->fix.visual) 
	{
	case FB_VISUAL_TRUECOLOR:
		/*
		 * 12 or 16-bit True Colour.  We encode the RGB value
		 * according to the RGB bitfield information.
		 */
		if (regno < 16) {
			u32 *pal = info->pseudo_palette;

			val  = chan_to_field(red,   &info->var.red);
			val |= chan_to_field(green, &info->var.green);
			val |= chan_to_field(blue,  &info->var.blue);

			pal[regno] = val;
			ret = 0;
		}//if
		break;

	case FB_VISUAL_STATIC_PSEUDOCOLOR:
	case FB_VISUAL_PSEUDOCOLOR:
		break;
	}//switch

	return ret;
}//vfb_setcolreg


/*
 *  Pan or Wrap the Display
 *
 *  This call looks only at xoffset, yoffset and the FB_VMODE_YWRAP flag
 */
static int vfb_pan_display(struct fb_var_screeninfo *var,
			   struct fb_info *info)
{
	if (var->vmode & FB_VMODE_YWRAP) 
	{
		if (var->yoffset < 0
		    || var->yoffset >= info->var.yres_virtual
		    || var->xoffset)
			return -EINVAL;
	}//if 
	else 
	{
		if (var->xoffset + var->xres > info->var.xres_virtual ||
		    var->yoffset + var->yres > info->var.yres_virtual)
			return -EINVAL;
	}//else

	info->var.xoffset = var->xoffset;
	info->var.yoffset = var->yoffset;
	if (var->vmode & FB_VMODE_YWRAP)
		info->var.vmode |= FB_VMODE_YWRAP;
	else
		info->var.vmode &= ~FB_VMODE_YWRAP;

	return 0;
}//vfb_pan_display


#ifndef MODULE
/*
 * The virtual framebuffer driver is only enabled if explicitly
 * requested by passing 'video=vfb:' (or any actual options).
 */
static int __init vfb_setup(char *options)
{
	char *this_opt;

	// set default options
	vfb_enab = 1;
	vfb_addr = STM32FB_FB_PHYS;
	vfb_size = STM32FB_FB_LEN;
	
	// is there any options ?
	if (!options || !*options)
		return 0;

	// scan all options
	while ((this_opt = strsep(&options, ",")) != NULL) 
	{	
		// it is null. go on...
		if (!*this_opt) continue;

		// Test all options
		if (!strcmp(this_opt, "disable"))
			vfb_enab = 0;
		else if (!strncmp(this_opt, "fbmem:", 6))
			vfb_addr = simple_strtoul(this_opt+6, NULL, 0);
		else if (!strncmp(this_opt, "fbsize:", 7))
			vfb_size = simple_strtoul(this_opt+7, NULL, 0);

		// debug print
		DEBUG("VFB : opt [%s]\n", this_opt);
	}//while

	return 0;
}//vfb_setup
#endif // MODULE



/*
*********************************************************************************************************
*                                         	 INITIALISATION
*********************************************************************************************************
*/

/*******************************************************************************
* Function Name  : init_vals
* Description    : None
* Input          : None
* Output         : None
* Return         : None
*******************************************************************************/
static int __init init_vals(struct fb_info *info)
{
    struct fb_var_screeninfo *var = &info->var;
    struct fb_fix_screeninfo *fix = &info->fix;

	var->xres 			= 240;
	var->yres 			= 320;
	var->xres_virtual   = 240;
	var->yres_virtual   = 320;
	var->xoffset		= 0;
	var->yoffset		= 0;
	var->bits_per_pixel = 16;
    var->grayscale		= 0;
    var->red.offset 	= 11;
    var->red.length 	= 5;
    var->green.offset 	= 5;
    var->green.length 	= 6;
    var->blue.offset 	= 0;
    var->blue.length 	= 5;
    var->activate 		= FB_ACTIVATE_NOW;
    var->height 		= -1;
    var->width 			= -1;
   	var->vmode 			= FB_VMODE_NONINTERLACED;
   	var->rotate			= 0;

	strcpy(fix->id, "STM32 VFB");
	fix->type 		 	= FB_TYPE_PACKED_PIXELS;
	fix->visual 	 	= FB_VISUAL_TRUECOLOR;
	fix->xpanstep 	 	= 0;
	fix->ypanstep 	 	= 1;
	fix->ywrapstep   	= 0;
	fix->line_length 	= var->xres_virtual * (var->bits_per_pixel/8);
	fix->accel 		 	= FB_ACCEL_NONE;

	return fb_alloc_cmap(&(info->cmap), 256, 0);
}//init_vals


/*******************************************************************************
* Function Name  : vfb_probe
* Description    : None
* Input          : None
* Output         : None
* Return         : None
*******************************************************************************/
static int __init vfb_probe(struct platform_device *dev)
{	
	struct fb_info *info;
	int retval = -ENOMEM;

	/*
	 * Reserve the framebuffer memory.
	 */
    if (!request_mem_region(vfb_addr, 
    						vfb_size, 
    						"STM32F4 Framebuffer")) 
    {
        printk(KERN_ERR "vfb: unable to reserve "
               "framebuffer at 0x%0x\n", (unsigned int)vfb_addr);
        retval = -EBUSY;
        goto fail_reqmem;
    }//if

	/*
	 * Allocate framebuffer info structure.
	 */
	info = framebuffer_alloc(sizeof(struct fb_info), &dev->dev);
	if (!info)
    {
        printk(KERN_ERR "vfb: unable to reserve framebuffer info\n");
        retval = -ENOMEM;
		goto fail_fballoc;
    }//if

	/*
	 * For real video cards we use ioremap.
	 */
	info->screen_base = ioremap(vfb_addr, vfb_size);
    if (!info->screen_base)
    {
        printk(KERN_ERR "vfb: unable to map framebuffer\n");
        retval = -ENOMEM;
        goto fail_remap;
    }//if

	/*
	 * Assign the frame buffer data
	 */
	info->screen_size = vfb_size;
	info->fbops = &vfb_ops;

	/*
	 * VFB must clear memory to prevent kernel info
	 * leakage into userspace
	 * VGA-based drivers MUST NOT clear memory if
	 * they want to be able to take over vgacon
	 */
	memset(info->screen_base, 0, info->screen_size);

	retval = fb_find_mode(&info->var, info, NULL, NULL, 0, NULL, 8);
	if ((!retval) || (retval == 4))
		memcpy(&(info->var), &vfb_default, sizeof(struct fb_var_screeninfo));

	vfb_fix.smem_start = vfb_addr;
	vfb_fix.smem_len   = vfb_size;
	memcpy(&(info->fix), &vfb_fix, sizeof(struct fb_fix_screeninfo));

	/*
	 * Allocate pseudo palette data
	 */
	info->pseudo_palette = kmalloc(sizeof(u32) * 16, GFP_KERNEL);
	if (!info->pseudo_palette)
    {
        printk(KERN_ERR "vfb: unable to reserve palette memory\n");
        retval = -ENOMEM;
        goto fail_palette;
    }//if

	//info->pseudo_palette = info->par;
	info->par = NULL;
	info->flags = FBINFO_FLAG_DEFAULT;

    /* 
     * We expect the boot loader to have initialized the chip
     * with appropriate parameters from which we can determinte
     * the flavor of lcd panel attached 
     */
   	retval = init_vals(info);
    if (retval < 0)
    {
        printk(KERN_ERR "vfb: unable to reserve cmap memory\n");
        goto fail_cmap;
    }//if

	/*
	 * Regiser the framebuffer
	 */
	retval = register_framebuffer(info);
	if (retval < 0)
    {
        printk(KERN_ERR "vfb: unable to register framebuffer\n");
		goto fail_register;
    }//if

	/*
	 * Assign the driver data
	 */
	platform_set_drvdata(dev, info);

	/*
	 * Everything is OK...
	 */
	printk(KERN_INFO
	       "fb%d: Virtual frame buffer device, using %ldK of video memory\n",
	       info->node, info->screen_size >> 10);
	
	return 0;

	/*
	 * There is an error!
	 */
fail_register:
	fb_dealloc_cmap(&info->cmap);
fail_cmap:
	kfree(info->pseudo_palette);
fail_palette:	
    iounmap(info->screen_base);
fail_remap:
	framebuffer_release(info);
fail_fballoc:
	release_mem_region(vfb_addr, vfb_size);
fail_reqmem:

	return retval;
}//vfb_probe


/*******************************************************************************
* Function Name  : vfb_remove
* Description    : None
* Input          : None
* Output         : None
* Return         : None
*******************************************************************************/
static int vfb_remove(struct platform_device *dev)
{
	struct fb_info *info = platform_get_drvdata(dev);

	if (info) 
	{
		unregister_framebuffer(info);
		fb_dealloc_cmap(&(info->cmap));
		kfree(info->pseudo_palette);
        iounmap(info->screen_base);
		framebuffer_release(info);
	}//if
	release_mem_region(vfb_addr, vfb_size);

	return 0;
}//vfb_remove


// platform driver instance
static struct platform_driver vfb_driver = 
{
	.probe	= vfb_probe,
	.remove = vfb_remove,
	.driver = { .name = "vfb" },
};

// platform device instance
static struct platform_device *vfb_device;


/*******************************************************************************
* Function Name  : vfb_init
* Description    : None
* Input          : None
* Output         : None
* Return         : None
*******************************************************************************/
static int __init vfb_init(void)
{
	int ret = 0;

#ifndef MODULE
	char *option = NULL;
	
	if (fb_get_options("vfb", &option))
		return -ENODEV;

	// parse all options
	vfb_setup(option);
#endif

	if (!vfb_enab)
		return -ENXIO;

	// registe the driver
	if ((ret = platform_driver_register(&vfb_driver)) == 0)
	{
		if ((vfb_device = platform_device_alloc("vfb", 0)) != 0)
			ret = platform_device_add(vfb_device);
		else
			ret = -ENOMEM;

		if (ret != 0) 
		{
			platform_device_put(vfb_device);
			platform_driver_unregister(&vfb_driver);
		}//if
	}//if

	return ret;
}//vfb_init


/*******************************************************************************
* Function Name  : vfb_exit
* Description    : None
* Input          : None
* Output         : None
* Return         : None
*******************************************************************************/
#ifdef MODULE
static void __exit vfb_exit(void)
{
	platform_device_unregister(vfb_device);
	platform_driver_unregister(&vfb_driver);
}//vfb_exit
module_exit(vfb_exit);
#endif
module_init(vfb_init);

MODULE_AUTHOR("tmk <tmk@robutest.com>");
MODULE_DESCRIPTION("Virtual Framebuffer driver for STM32F4 LTDC");
MODULE_LICENSE("GPL");
