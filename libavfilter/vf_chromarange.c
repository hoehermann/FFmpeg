/*
 * Copyright (c) 2020 Hermann Höhne <hermann@hehoe.org>
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include "libavutil/opt.h"
#include "libavutil/imgutils.h"
#include "libavutil/intreadwrite.h"
#include "avfilter.h"
#include "formats.h"
#include "internal.h"
#include "video.h"

typedef struct ChromarangeContext {
    const AVClass *class; // av_log wants the first field of the struct to be the pointer to the class

    // parameters given by user
    int y_lower;
    int u_lower;
    int v_lower;
    int y_upper;
    int u_upper;
    int v_upper;
    int squaredsat_lower;
    //enum AVPixelFormat pix_fmt;

    // parameters defined by output format
    int depth;
    int hsub_log2;
    int vsub_log2;

    // for processing
    int (*do_slice)(AVFilterContext *ctx, void *arg, int jobnr, int nb_jobs);
} ChromarangeContext;

static int do_chromarange_slice(AVFilterContext *avctx, void *arg, int jobnr, int nb_jobs)
{
    AVFrame *frame = arg;

    const int slice_start = (frame->height * jobnr) / nb_jobs;
    const int slice_end = (frame->height * (jobnr + 1)) / nb_jobs;

    ChromarangeContext *ctx = avctx->priv;

    for (int row = slice_start; row < slice_end; ++row) {
        for (int col = 0; col < frame->width; ++col) {

            int ucol = col >> ctx->hsub_log2;
            int vrow = row >> ctx->vsub_log2;

            int y = frame->data[0][frame->linesize[0] * row + col];
            int u = frame->data[1][frame->linesize[1] * vrow + ucol];
            int v = frame->data[2][frame->linesize[2] * vrow + ucol];

            int ud = u-128;
            int vd = v-128;
            int sqaredsat = ud*ud+vd*vd;

            int transparent =
                u >= ctx->u_lower && u <= ctx->u_upper &&
                v >= ctx->v_lower && v <= ctx->v_upper &&
                y >= ctx->y_lower && y <= ctx->y_upper &&
                sqaredsat >= ctx->squaredsat_lower;
            int alpha = !transparent * 255;
            frame->data[3][frame->linesize[3] * row + col] = alpha;
        }
    }

    return 0;
}

static int filter_frame(AVFilterLink *link, AVFrame *frame)
{
    AVFilterContext *avctx = link->dst;
    ChromarangeContext *ctx = avctx->priv;
    int res;

    if (res = avctx->internal->execute(avctx, ctx->do_slice, frame, NULL, FFMIN(frame->height, ff_filter_get_nb_threads(avctx))))
        return res;

    return ff_filter_frame(avctx->outputs[0], frame);
}

static av_cold int config_output(AVFilterLink *outlink)
{
    const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get(outlink->format);
    AVFilterContext *avctx = outlink->src;
    ChromarangeContext *ctx = avctx->priv;
    ctx->depth = desc->comp[0].depth;
    if (ctx->depth != 8) {
        // this should not happen due to the restrictions in pixel_fmts, but I want to be safe
        av_log(ctx, AV_LOG_ERROR, "This filter does only handle 8 bit YUV, but configured output is %d bit.\n", ctx->depth );
        return AVERROR(ENOMEM);
    }
    ctx->do_slice = do_chromarange_slice;
    return 0;
}

static av_cold int query_formats(AVFilterContext *avctx)
{
    static const enum AVPixelFormat pixel_fmts[] = {
        AV_PIX_FMT_YUVA420P,
        AV_PIX_FMT_YUVA422P,
        AV_PIX_FMT_YUVA444P,
        AV_PIX_FMT_NONE
    };
    // add YUV to AV_PIX_FMT_GRAY8 for single-channel matte without alphaextract?

    AVFilterFormats *formats = NULL;

    formats = ff_make_format_list(pixel_fmts);
    if (!formats) {
        return AVERROR(ENOMEM);
    }

    return ff_set_common_formats(avctx, formats);
}

static av_cold int config_input(AVFilterLink *inlink)
{
    AVFilterContext *avctx = inlink->dst;
    ChromarangeContext *ctx = avctx->priv;
    const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get(inlink->format);

    ctx->hsub_log2 = desc->log2_chroma_w;
    ctx->vsub_log2 = desc->log2_chroma_h;

    if (ctx->hsub_log2 > 0 || ctx->vsub_log2 > 0) {
        av_log(ctx, AV_LOG_WARNING, "Selected output format leads to sub-sampled chroma on input. Try to force yuv444p for better quality.\n");
    }
    return 0;
}

static int process_command(AVFilterContext *ctx, const char *cmd, const char *args,
                           char *res, int res_len, int flags)
{
    int ret;

    ret = ff_filter_process_command(ctx, cmd, args, res, res_len, flags);
    if (ret < 0)
        return ret;

    return config_output(ctx->outputs[0]);
}

static const AVFilterPad chromarange_inputs[] = {
    {
        .name           = "default",
        .type           = AVMEDIA_TYPE_VIDEO,
        .needs_writable = 1,
        .filter_frame   = filter_frame,
        .config_props   = config_input,
    },
    { NULL }
};

static const AVFilterPad chromarange_outputs[] = {
    {
        .name           = "default",
        .type           = AVMEDIA_TYPE_VIDEO,
        .config_props   = config_output,
    },
    { NULL }
};

#define OFFSET(x) offsetof(ChromarangeContext, x)
#define FLAGS AV_OPT_FLAG_FILTERING_PARAM|AV_OPT_FLAG_VIDEO_PARAM|AV_OPT_FLAG_RUNTIME_PARAM

static const AVOption chromarange_options[] = {
    { "ylower", "luminance Y lower bound", OFFSET(y_lower), AV_OPT_TYPE_INT, { .i64 = 64 }, 0, 255, FLAGS },
    { "ulower", "chroma U lower bound", OFFSET(u_lower), AV_OPT_TYPE_INT, { .i64 = 128 }, 0, 255, FLAGS },
    { "vlower", "chroma V lower bound", OFFSET(v_lower), AV_OPT_TYPE_INT, { .i64 = 0 }, 0, 255, FLAGS },
    { "yupper", "luminance Y upper bound", OFFSET(y_upper), AV_OPT_TYPE_INT, { .i64 = 255 }, 0, 255, FLAGS },
    { "uupper", "chroma U upper bound", OFFSET(u_upper), AV_OPT_TYPE_INT, { .i64 = 255 }, 0, 255, FLAGS },
    { "vupper", "chroma V upper bound", OFFSET(v_upper), AV_OPT_TYPE_INT, { .i64 = 127 }, 0, 255, FLAGS },
    { "minsat", "minimum saturation as suqared euclidean UV distance from grey (128,128)", OFFSET(squaredsat_lower), AV_OPT_TYPE_INT, { .i64 = 265 }, 0, 128*128, FLAGS },
    //{"pixel_format", "pixel format", OFFSET(pix_fmt), AV_OPT_TYPE_PIXEL_FMT, {.i64=AV_PIX_FMT_NONE}, -1, INT_MAX, 0 },
    { NULL }
};

AVFILTER_DEFINE_CLASS(chromarange);

AVFilter ff_vf_chromarange = {
    .name          = "chromarange",
    .description   = NULL_IF_CONFIG_SMALL("Turns a certain color range into transparency. Operates on YUV colors."),
    .priv_size     = sizeof(ChromarangeContext),
    .priv_class    = &chromarange_class,
    .query_formats = query_formats,
    .inputs        = chromarange_inputs,
    .outputs       = chromarange_outputs,
    .flags         = AVFILTER_FLAG_SUPPORT_TIMELINE_GENERIC | AVFILTER_FLAG_SLICE_THREADS,
    .process_command = process_command,
};
